module Interpreter.Interpreter (
    Interpreter
  , InterpreterT(..)
  , runInterpreter
  , execute
  , interpret
  , as

  -- * set imports
  , setImports
  , setImportsQ
  , setImportsF
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Either
import Data.IORef
import Data.Typeable

import qualified GHC
import qualified GhcMonad as GHC
import qualified Exception as GHC
import qualified DynFlags as GHC
import qualified GHC.Exts as GHC
import qualified Parser as GHC
import qualified StringBuffer as GHC
import qualified Lexer as GHC
import qualified ErrUtils as GHC
import qualified SrcLoc as GHC
import qualified FastString as GHC
import qualified HscTypes as GHC
import qualified Outputable as GHC

import Interpreter.Session

newtype InterpreterT m a = InterpreterT { unInterpreterT :: ReaderT InterpreterSession (GhcT m) a }
    deriving (Functor, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance (Monad m) => Applicative (InterpreterT m) where
    pure  = return
    (<*>) = ap

instance MonadTrans InterpreterT where
    lift = InterpreterT . lift . lift

instance (MonadIO m, MonadCatch m, MonadMask m, GHC.ExceptionMonad m) => GHC.ExceptionMonad (InterpreterT m) where
    gcatch = catch
    gmask = mask

type Interpreter = InterpreterT IO

type MonadInterpreter m = (MonadMask m, GHC.ExceptionMonad m)

newtype GhcT m a = GhcT { unGhcT :: GHC.GhcT m a }
                 deriving (Functor, Monad, GHC.HasDynFlags)

instance (Functor m, Monad m) => Applicative (GhcT m) where
    pure  = return
    (<*>) = ap

instance MonadTrans GhcT where
    lift = GhcT . lift

instance MonadTrans GHC.GhcT where
    lift = GHC.liftGhcT

instance MonadIO m => MonadIO (GhcT m) where
    liftIO = GhcT . GHC.liftIO

instance MonadCatch m => MonadThrow (GhcT m) where
    throwM = lift . throwM

instance (MonadIO m, MonadCatch m, MonadMask m, GHC.ExceptionMonad m) => MonadCatch (GhcT m) where
    m `catch` f = GhcT (unGhcT m `GHC.gcatch` (unGhcT . f))

instance (MonadIO m, MonadMask m, GHC.ExceptionMonad m) => MonadMask (GhcT m) where
    mask f = wrap $ \s ->
               mask $ \io_restore ->
                 unwrap (f $ \m -> (wrap $ \s' -> io_restore (unwrap m s'))) s
      where
        wrap g   = GhcT $ GHC.GhcT g
        unwrap m = GHC.unGhcT (unGhcT m)

    uninterruptibleMask f = wrap $ \s ->
                              uninterruptibleMask $ \io_restore ->
                                unwrap (f $ \m -> (wrap $ \s' -> io_restore (unwrap m s'))) s
      where
        wrap g   = GhcT $ GHC.GhcT g
        unwrap m = GHC.unGhcT (unGhcT m)

    generalBracket acquire release body
      = wrap $ \s -> generalBracket (unwrap acquire s)
                                    (\a exitCase -> unwrap (release a exitCase) s)
                                    (\a -> unwrap (body a) s)
      where
        wrap g   = GhcT $ GHC.GhcT g
        unwrap m = GHC.unGhcT (unGhcT m)

instance (MonadIO m, MonadCatch m, MonadMask m, GHC.ExceptionMonad m) => GHC.ExceptionMonad (GhcT m) where
    gcatch = catch
    gmask = mask

instance (Functor m, MonadIO m, MonadMask m, GHC.ExceptionMonad m) => GHC.GhcMonad (GhcT m) where
    getSession = GhcT GHC.getSession
    setSession = GhcT . GHC.setSession

-- | remember to setSessionDynFlags before calling this `execute` method
execute :: MonadInterpreter m => FilePath -> GHC.Session -> InterpreterSession -> InterpreterT m a -> m (Either InterpreterError a)
execute libdir gsession isession = try . runGhcT (Just libdir) gsession . flip runReaderT isession . unInterpreterT

runInterpreter :: MonadInterpreter m => FilePath -> InterpreterT m a -> m (Either InterpreterError a)
runInterpreter libdir action = do
    isession <- newInterpreterSession `catch` rethrowGhcException
    ref <- liftIO $ newIORef (error "empty session")
    let gsession = GHC.Session ref
    execute libdir gsession isession (InterpreterT (lift initialize) >> action)
  where
    rethrowGhcException = throwM . GhcException . showGhcEx

    initialize = do
        df0 <- GHC.getSessionDynFlags
        GHC.setSessionDynFlags df0

showGhcEx :: GHC.GhcException -> String
showGhcEx = flip GHC.showGhcException ""

runGhcT :: MonadInterpreter m => Maybe String -> GHC.Session -> GhcT m a -> m a
runGhcT libdir session ghct = do
    flip GHC.unGhcT session $ do
        GHC.initGhcMonad libdir
        GHC.withCleanupSession $ unGhcT ghct

interpret :: (MonadInterpreter m, Typeable a) => String -> a -> InterpreterT m a
interpret expr typ = do
    runParser GHC.parseStmt expr
    let exprTypeSig = concat ["(", expr, ") :: ", show $ Data.Typeable.typeOf typ]
    expr_val <- mayFail $ InterpreterT $ lift $ fmap Just $ GHC.compileExpr exprTypeSig
    --
    return (GHC.unsafeCoerce# expr_val :: a)

-- | Convenience functions to be used with @interpret@ to provide witnesses.
--   Example:
--
--   * @interpret \"head [True,False]\" (as :: Bool)@
--
--   * @interpret \"head $ map show [True,False]\" infer >>= flip interpret (as :: Bool)@
as, infer :: Typeable a => a
as    = undefined
infer = undefined

mayFail :: MonadInterpreter m => InterpreterT m (Maybe a) -> InterpreterT m a
mayFail action = do
    maybe_res <- action

    es <- modifySessionRef ghcErrorList (const [])

    case (maybe_res, null es) of
        (Nothing, True)  -> throwM $ UnknownError "Got no error message"
        (Nothing, False) -> throwM $ WontCompile (reverse es)
        (Just a, _)      -> return a

data ParseResult = ParseOk | ParseError GHC.SrcSpan GHC.MsgDoc

runParser :: MonadInterpreter m => GHC.P a -> String -> InterpreterT m ParseResult
runParser parser expr = do
    dynFlags <- InterpreterT $ lift $ GhcT GHC.getSessionDynFlags
    let buf = GHC.stringToStringBuffer expr
    let srcLoc = GHC.mkRealSrcLoc (GHC.fsLit "<customized interpreter>") 1 1
    let parse_res = GHC.unP parser (GHC.mkPState dynFlags buf srcLoc)
    --
    case parse_res of
        GHC.POk{}            -> return ParseOk
        GHC.PFailed pst      -> let errMsgs = GHC.getErrorMessages pst dynFlags
                                    span = foldr (GHC.combineSrcSpans . GHC.errMsgSpan) GHC.noSrcSpan errMsgs
                                    err = GHC.vcat $ GHC.pprErrMsgBagWithLoc errMsgs
                                in pure (ParseError span err)

modifySessionRef :: MonadInterpreter m => (InterpreterSession -> IORef a) -> (a -> a) -> InterpreterT m a
modifySessionRef target f = do
    ref <- InterpreterT $ asks target
    liftIO $ atomicModifyIORef ref (\a -> (f a, a))

-----------------------------------------------------
---------------------- Imports ----------------------
-----------------------------------------------------

-- | Sets the modules whose exports must be in context.
--
--   Warning: 'setImports', 'setImportsQ', and 'setImportsF' are mutually exclusive.
--   If you have a list of modules to be used qualified and another list
--   unqualified, then you need to do something like
--
--   >  setImportsQ ((zip unqualified $ repeat Nothing) ++ qualifieds)
setImports :: MonadInterpreter m => [ModuleName] -> InterpreterT m ()
setImports ms = setImportsF $ map (\m -> ModuleImport m NotQualified NoImportList) ms

-- | Sets the modules whose exports must be in context; some
--   of them may be qualified. E.g.:
--
--   @setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M")]@.
--
--   Here, "map" will refer to Prelude.map and "M.map" to Data.Map.map.
setImportsQ :: MonadInterpreter m => [(ModuleName, Maybe String)] -> InterpreterT m ()
setImportsQ ms = setImportsF $ map (\(m,q) -> ModuleImport m (maybe NotQualified (QualifiedAs . Just) q) NoImportList) ms

-- | Sets the modules whose exports must be in context; some
--   may be qualified or have imports lists. E.g.:
--
--   @setImportsF [ModuleImport "Prelude" NotQualified NoImportList, ModuleImport "Data.Text" (QualifiedAs $ Just "Text") (HidingList ["pack"])]@

setImportsF :: MonadInterpreter m => [ModuleImport] -> InterpreterT m ()
setImportsF ms = do
    -- check that the imports exist
    regularMods <- mapM (findModule . modName) regularImports
    mapM_ (findModule . modName) phantomImports

    -- add qualified imports
    -- new_pm <- if null phantomImports
    --     then return Nothing
    --     else do
    --         new_pm <- addPhantomModule $ \mod_name -> unlines $
    --                 ("module " ++ mod_name ++ " where ") :
    --                 map newImportLine phantomImports
    --         onState (\s -> s{importQualHackMod = Just new_pm})
    --         return $ Just new_pm

    modifySessionRef interpreterState $ \s -> s { qualImports = qualImports s ++ ms }
    -- add imports to GHC
    mods <- mapM (findModule . modName) ms
    InterpreterT $ lift $ setContext mods 
    return ()
    where
        (regularImports, phantomImports) = partitionEithers $ map (\m -> if isPhantomModule m then Right m else Left m) ms

        findModule :: MonadInterpreter m => ModuleName -> InterpreterT m GHC.Module
        findModule mn = InterpreterT $ lift $ GHC.findModule (GHC.mkModuleName mn) Nothing


setContext :: GHC.GhcMonad m => [GHC.Module] -> m ()
setContext ms = GHC.setContext $ map (GHC.IIDecl . GHC.simpleImportDecl . GHC.moduleName) ms
