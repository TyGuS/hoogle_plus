module Interpreter where

import Control.Concurrent
import Control.Exception
import Control.Monad (unless, forever)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Exception ( gtry )
import qualified EnumSet                       as ES
import           GHC                     hiding ( Id )
import           GHC.LanguageExtensions.Type    ( Extension
                                                  ( ExtendedDefaultRules
                                                  , FlexibleContexts
                                                  , ScopedTypeVariables
                                                  )
                                                )
import           GHC.Paths                      ( libdir )
import           Text.Printf                    ( printf )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.Text as Text
import Unsafe.Coerce (unsafeCoerce)
import Data.Dynamic
import System.Timeout
import System.Exit
import HscMain
import HscTypes

import Control.Concurrent.Async

import           Paths_HooglePlus
import Database.Dataset

msgSize :: Int
msgSize = 999999999

portNumber :: String
portNumber = "1234"

frameworkModules :: [(String, Maybe String)]
frameworkModules =
  zip
      [ "Test.SmallCheck"
      , "Test.SmallCheck.Drivers"
      , "Test.LeanCheck.Function.ShowFunction"
      , "System.IO.Silently"
      , "System.Timeout"
      , "Control.Exception"
      , "Control.Monad"
      , "Control.Monad.State"
      ]
      (repeat Nothing)

    ++ [("Test.ChasingBottoms", Just "CB")]

main :: IO ()
main = do
    srcPath <- getDataFileName "InternalTypeGen.hs"

    hscEnv <- runGhc (Just libdir) $ do
        -- do the interpreter initialization
        dflags <- getSessionDynFlags
        let dflags' = dflags { hscTarget = HscInterpreted
                             , ghcLink   = LinkInMemory
                             , generalFlags   = ES.delete Opt_OmitYields $ generalFlags dflags
                             , extensionFlags = ES.insert ScopedTypeVariables $ ES.insert ExtendedDefaultRules $ ES.insert FlexibleContexts $ extensionFlags dflags
                             }
        _ <- setSessionDynFlags dflags'

        target <- guessTarget srcPath Nothing
        setTargets [target]
        res <- load LoadAllTargets

        if succeeded res
            then liftIO $ putStrLn "load module succeeded"
            else error "load module failed"

        modGraph <- getModuleGraph
        let modSummaries = mgModSummaries modGraph
        let mdlName = ms_mod_name (head modSummaries)
        imports <- prepareModules (frameworkModules ++ zip (map Text.unpack includedModules) (repeat Nothing))
        setContext (IIModule mdlName : imports)

        getSession

    -- run the interpreter inside the GHC monad
    sock <- liftIO $ withSocketsDo $ do
        addr <- resolve
        bracketOnError (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)) close $ \sock -> do
            setSocketOption sock ReuseAddr 1
            withFdSocket sock setCloseOnExecIfNeeded
            bind sock $ addrAddress addr
            listen sock msgSize

            putStrLn "server is established"
            return sock

    timeout (120 * 10 ^ 6) $ do
        forever $ bracketOnError (accept sock) (close . fst) $ \(conn, _) -> do
            -- forkFinally (talk hscEnv conn) (\_ -> gracefulClose conn 5000 >> print "connection closed")
            withAsync (talk hscEnv conn) (\a -> wait a >> gracefulClose conn 5000 >> print "connection closed")
            -- forkFinally (withAsync (talk hscEnv conn) wait) (\_ -> gracefulClose conn 5000 >> print "connection closed")
            -- forever $ do
            --     (conn, _) <- liftIO $ accept sock
            --     talk conn
    return ()
  where
    resolve = do
        let hints = defaultHints {
                      addrFlags = [AI_PASSIVE]
                    , addrSocketType = Stream
                    }
        head <$> getAddrInfo (Just hints) Nothing (Just portNumber)

    session :: HscEnv -> Ghc a -> IO a
    session hscEnv m = runGhc (Just libdir) $ do
        setSession (mkInteractiveHscEnv hscEnv)
        x <- m
        -- dflags <- getSessionDynFlags
        -- liftIO (newHscEnv dflags) >>= setSession
        return x

    talk hscEnv s = do
        prog <- liftIO $ recv s msgSize
        let progStr = C.unpack prog
        liftIO $ putStrLn $ "Received: " ++ progStr
        unless (S.null prog) $ do
            -- msg <- do
            liftIO $ print "start"
            msgOrErr <- try $ session hscEnv $ do
                -- let timeoutStr = "timeout (2 * 10^(6 :: Int)) (" ++ progStr ++ ")"
                (dynRes :: Either SomeException (Maybe (IO String))) <- gtry (fromDynamic <$> dynCompileExpr progStr)
                case dynRes of
                    Left e -> return (Left $ show e)
                    Right dyn -> case dyn of
                        Nothing -> error "unsupported expression"
                        Just act -> do
                            !res <- liftIO (timeout (5 * 10 ^ (6 :: Int)) act)
                            case res of
                                Nothing -> return (Left "timeout")
                                Just res' -> return (Right res')

            let msg = case msgOrErr of
                                Left (e :: SomeException) -> Left (show e)
                                Right x -> x

            liftIO $ putStrLn $ "Sending from server: " ++ show msg
            liftIO $ sendAll s (LS.toStrict $ S.toLazyByteString $ S.string8 $ show msg)

        -- killThread =<< myThreadId

    prepareModules mdls = do
        let imports = map buildImportDecl mdls
        decls <- mapM parseImportDecl imports
        return (map IIDecl decls)

    buildImportDecl :: (String, Maybe String) -> String
    buildImportDecl (mdl, mbAlias) =
        let alias = maybe "" (\a -> " as " ++ a) mbAlias
        in printf "import %s%s" mdl alias