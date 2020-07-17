{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TopDown.Synthesize(synthesize, envToGoal) where

import Database.Environment
import Database.Util
import qualified HooglePlus.Abstraction as Abstraction
import PetriNet.PNSolver
import HooglePlus.TypeChecker
import HooglePlus.GHCChecker (check)
import Synquid.Error
import Synquid.Logic
import Synquid.Parser
import Synquid.Pretty
import Synquid.Program
import Synquid.Resolver
import Synquid.Type
import Synquid.Util
import Types.CheckMonad
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Filtering
import Types.Program
import Types.Solver
import Types.TypeChecker
import Types.Type
import Types.IOFormat
import HooglePlus.Utils
import HooglePlus.IOFormat
import Examples.ExampleChecker
import PetriNet.Util

import Control.Applicative ((<$>))
import Control.Concurrent.Chan
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.List
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock
import Data.Time.Format
import System.CPUTime
import System.Exit
import Text.Parsec.Indent
import Text.Parsec.Pos
import Text.Printf (printf)


envToGoal :: Environment -> String -> IO Goal
envToGoal env queryStr = do
  let transformedSig = "goal :: " ++ queryStr ++ "\ngoal = ??"
  let parseResult = flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" transformedSig
  case parseResult of
    Left parseErr -> let e = toErrorMessage parseErr
                      in putDoc (pretty e) >> putDoc linebreak >> error (prettyShow e)
    Right (funcDecl:decl:_) -> case decl of
      Pos _ (SynthesisGoal id uprog) -> do
        let Pos _ (FuncDecl _ sch) = funcDecl
        let goal = Goal id env sch uprog 3 $ initialPos "goal"
        let spec = runExcept $ evalStateT (resolveSchema (gSpec goal)) (initResolverState { _environment = env })
        case spec of
          Right sp -> do
            let (env', monospec) = updateEnvWithBoundTyVars sp env
            let (env'', destinationType) = updateEnvWithSpecArgs monospec env'
            return $ goal { gEnvironment = env'', gSpec = sp }
          Left parseErr -> putDoc (pretty parseErr) >> putDoc linebreak >> error (prettyShow parseErr)
      _ -> error "parse a signature for a none goal declaration"

synthesize :: SearchParams -> Goal -> [Example] -> Chan Message -> IO ()
synthesize searchParams goal examples messageChan = do
    let rawEnv = gEnvironment goal
    let goalType = gSpec goal :: RSchema
    let destinationType = lastType (toMonotype goalType)
    let useHO = _useHO searchParams
    let rawSyms = rawEnv ^. symbols
    let hoCands = rawEnv ^. hoCandidates
    envWithHo <- do
    
    --------------------------
    -- HIGHER ORDER STUFF 
    -- envWithHo <- if useHO -- add higher order query arguments
    --     then do
    --         let args = rawEnv ^. arguments
    --         let hoArgs = Map.filter (isFunctionType . toMonotype) args
    --         let hoFuns = map (\(k, v) -> (k ++ hoPostfix, withSchema toFunType v)) (Map.toList hoArgs)
    --         return $ rawEnv { 
    --             _symbols = rawSyms `Map.union` Map.fromList hoFuns, 
    --             _hoCandidates = hoCands ++ map fst hoFuns
    --             }
    --     else do
    --------------------------

     --------------------------
      let args = rawEnv ^. arguments
      let hoArgs = Map.filter (isFunctionType . toMonotype) args
      let hoFuns = map (\(k, v) -> (k ++ hoPostfix, withSchema toFunType v)) (Map.toList hoArgs)
      return $ rawEnv { 
          _symbols = rawSyms `Map.union` Map.fromList hoFuns, 
          _hoCandidates = hoCands ++ map fst hoFuns
          }
    --     else do
    --------------------------

      -- let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
      -- return $ rawEnv {
      --     _symbols = Map.withoutKeys syms $ Set.fromList hoCands, 
      --     _hoCandidates = []
      --     }

    -- start <- getCPUTime
    -- liftIO $ mapM print $ Map.toList (envWithHo ^. symbols)
    iterativeDeepening envWithHo messageChan searchParams examples goalType
    -- end <- getCPUTime

    -- let diff = fromIntegral (end - start) / (10^12)
    -- printf "Computation time: %0.3f sec\n" (diff :: Double)

    writeChan messageChan (MesgClose CSNormal)
    return ()



-- ("(Data.Bool.&&)",(Bool -> (Bool -> Bool)))
-- ("(Data.Bool.||)",(Bool -> (Bool -> Bool)))
-- ("(Data.Eq./=)",<a> . (@@hplusTC@@Eq (a) -> (a -> (a -> Bool))))
--  Eq a => a -> a -> Bool
-- ("(Data.Eq./=)_0'ho'",<a> . (@@hplusTC@@Eq (a) -> (a -> Fun (a) (Bool))))
--  Eq a => a -> (Fun a Bool)    a1000 ==> Fun a Bool
-- ("(Data.Eq./=)_1'ho'",<a> . (@@hplusTC@@Eq (a) -> Fun (a) ((Fun (a) (Bool)))))
--  Eq a => (Fun a (Fun a Bool))
-- ("(Data.Eq./=)_2'ho'",<a> . Fun ((@@hplusTC@@Eq (a))) ((Fun (a) ((Fun (a) (Bool))))))
--  Fun (Eq a) (Fun a (Fun a Bool))
-- ("(Data.Eq.==)",<a> . (@@hplusTC@@Eq (a) -> (a -> (a -> Bool))))
-- ("(Data.Eq.==)_0'ho'",<a> . (@@hplusTC@@Eq (a) -> (a -> Fun (a) (Bool))))
-- ("(Data.Eq.==)_1'ho'",<a> . (@@hplusTC@@Eq (a) -> Fun (a) ((Fun (a) (Bool)))))
-- ("(Data.Eq.==)_2'ho'",<a> . Fun ((@@hplusTC@@Eq (a))) ((Fun (a) ((Fun (a) (Bool))))))
-- ("(Data.Function.$)",<b> . <a> . (((a -> b)) -> (a -> b)))
-- ("(Data.Function.$)_0'ho'",<b> . <a> . (((a -> b)) -> Fun (a) (b)))
-- ("(Data.Function.$)_1'ho'",<b> . <a> . Fun ((Fun (a) (b))) ((Fun (a) (b))))
-- ("(Data.Function.&)",<b> . <a> . (a -> (((a -> b)) -> b)))
-- ("(Data.Function..)",<c> . <b> . <a> . (((b -> c)) -> (((a -> b)) -> (a -> c))))
-- ("(GHC.List.!!)",<a> . ([a] -> (Int -> a)))
-- ("(GHC.List.++)",<a> . ([a] -> ([a] -> [a])))
-- ("@@hplusTCInstance@@0EqBool",@@hplusTC@@Eq (Bool))
-- ("@@hplusTCInstance@@0EqChar",@@hplusTC@@Eq (Char))
-- ("@@hplusTCInstance@@0EqDouble",@@hplusTC@@Eq (Double))
-- ("@@hplusTCInstance@@0EqFloat",@@hplusTC@@Eq (Float))
-- ("@@hplusTCInstance@@0EqInt",@@hplusTC@@Eq (Int))
-- ("@@hplusTCInstance@@0EqUnit",@@hplusTC@@Eq (Unit))
-- ("@@hplusTCInstance@@0IsString",@@hplusTC@@IsString (Builder))
-- ("@@hplusTCInstance@@0NumDouble",@@hplusTC@@Num (Double))
-- ("@@hplusTCInstance@@0NumFloat",@@hplusTC@@Num (Float))
-- ("@@hplusTCInstance@@0NumInt",@@hplusTC@@Num (Int))
-- ("@@hplusTCInstance@@0OrdBool",@@hplusTC@@Ord (Bool))
-- ("@@hplusTCInstance@@0OrdChar",@@hplusTC@@Ord (Char))
-- ("@@hplusTCInstance@@0OrdDouble",@@hplusTC@@Ord (Double))
-- ("@@hplusTCInstance@@0OrdFloat",@@hplusTC@@Ord (Float))
-- ("@@hplusTCInstance@@0OrdInt",@@hplusTC@@Ord (Int))
-- ("@@hplusTCInstance@@0ShowBool",@@hplusTC@@Show (Bool))
-- ("@@hplusTCInstance@@0ShowChar",@@hplusTC@@Show (Char))
-- ("@@hplusTCInstance@@0ShowDouble",@@hplusTC@@Show (Double))
-- ("@@hplusTCInstance@@0ShowFloat",@@hplusTC@@Show (Float))
-- ("@@hplusTCInstance@@0ShowInt",@@hplusTC@@Show (Int))
-- ("@@hplusTCInstance@@0ShowUnit",@@hplusTC@@Show (Unit))
-- ("@@hplusTCInstance@@1Show",<b> . <a> . (@@hplusTC@@Show (a) -> (@@hplusTC@@Show (b) -> @@hplusTC@@Show ((Either (a) (b))))))
-- ("@@hplusTCInstance@@2Read",<b> . <a> . (@@hplusTC@@Read (a) -> (@@hplusTC@@Read (b) -> @@hplusTC@@Read ((Either (a) (b))))))
-- ("@@hplusTCInstance@@3Ord",<b> . <a> . (@@hplusTC@@Ord (a) -> (@@hplusTC@@Ord (b) -> @@hplusTC@@Ord ((Either (a) (b))))))
-- ("@@hplusTCInstance@@4Eq",<b> . <a> . (@@hplusTC@@Eq (a) -> (@@hplusTC@@Eq (b) -> @@hplusTC@@Eq ((Either (a) (b))))))
-- ("@@hplusTCInstance@@6Semigroup",<b> . <a> . @@hplusTC@@Semigroup ((Either (a) (b))))
-- ("@@hplusTCInstance@@9Eq",<a> . (@@hplusTC@@Eq (a) -> @@hplusTC@@Eq (([a]))))
-- ("Cons",<a> . (a -> ([a] -> {[a]|_v == (Cons x xs)})))
-- ("Data.Bool.False",Bool)
-- ("Data.Bool.True",Bool)
-- ("Data.Bool.bool",<a> . (a -> (a -> (Bool -> a))))
-- ("Data.Bool.not",(Bool -> Bool))
-- ("Data.Bool.otherwise",Bool)
-- ("Data.ByteString.Builder.byteString",(ByteString -> Builder))
-- ("Data.ByteString.Builder.byteStringHex",(ByteString -> Builder))
-- ("Data.ByteString.Builder.char7",(Char -> Builder))
-- ("Data.ByteString.Builder.char8",(Char -> Builder))
-- ("Data.ByteString.Builder.charUtf8",(Char -> Builder))
-- ("Data.ByteString.Builder.doubleBE",(Double -> Builder))
-- ("Data.ByteString.Builder.doubleDec",(Double -> Builder))
-- ("Data.ByteString.Builder.doubleHexFixed",(Double -> Builder))
-- ("Data.ByteString.Builder.doubleLE",(Double -> Builder))
-- ("Data.ByteString.Builder.floatBE",(Float -> Builder))
-- ("Data.ByteString.Builder.floatDec",(Float -> Builder))
-- ("Data.ByteString.Builder.floatHexFixed",(Float -> Builder))
-- ("Data.ByteString.Builder.floatLE",(Float -> Builder))
-- ("Data.ByteString.Builder.hPutBuilder",(Handle -> (Builder -> IO (Unit))))
-- ("Data.ByteString.Builder.int16BE",(Int16 -> Builder))
-- ("Data.ByteString.Builder.int16Dec",(Int16 -> Builder))
-- ("Data.ByteString.Builder.int16HexFixed",(Int16 -> Builder))
-- ("Data.ByteString.Builder.int16LE",(Int16 -> Builder))
-- ("Data.ByteString.Builder.int32BE",(Int32 -> Builder))
-- ("Data.ByteString.Builder.int32Dec",(Int32 -> Builder))
-- ("Data.ByteString.Builder.int32HexFixed",(Int32 -> Builder))
-- ("Data.ByteString.Builder.int32LE",(Int32 -> Builder))
-- ("Data.ByteString.Builder.int64BE",(Int64 -> Builder))
-- ("Data.ByteString.Builder.int64Dec",(Int64 -> Builder))
-- ("Data.ByteString.Builder.int64HexFixed",(Int64 -> Builder))
-- ("Data.ByteString.Builder.int64LE",(Int64 -> Builder))
-- ("Data.ByteString.Builder.int8",(Int8 -> Builder))
-- ("Data.ByteString.Builder.int8Dec",(Int8 -> Builder))
-- ("Data.ByteString.Builder.int8HexFixed",(Int8 -> Builder))
-- ("Data.ByteString.Builder.intDec",(Int -> Builder))
-- ("Data.ByteString.Builder.integerDec",(Integer -> Builder))
-- ("Data.ByteString.Builder.lazyByteString",(ByteString -> Builder))
-- ("Data.ByteString.Builder.lazyByteStringHex",(ByteString -> Builder))
-- ("Data.ByteString.Builder.shortByteString",(ShortByteString -> Builder))
-- ("Data.ByteString.Builder.string7",([Char] -> Builder))
-- ("Data.ByteString.Builder.string8",([Char] -> Builder))
-- ("Data.ByteString.Builder.stringUtf8",([Char] -> Builder))
-- ("Data.ByteString.Builder.toLazyByteString",(Builder -> ByteString))
-- ("Data.ByteString.Builder.word16BE",(Word16 -> Builder))
-- ("Data.ByteString.Builder.word16Dec",(Word16 -> Builder))
-- ("Data.ByteString.Builder.word16Hex",(Word16 -> Builder))
-- ("Data.ByteString.Builder.word16HexFixed",(Word16 -> Builder))
-- ("Data.ByteString.Builder.word16LE",(Word16 -> Builder))
-- ("Data.ByteString.Builder.word32BE",(Word32 -> Builder))
-- ("Data.ByteString.Builder.word32Dec",(Word32 -> Builder))
-- ("Data.ByteString.Builder.word32Hex",(Word32 -> Builder))
-- ("Data.ByteString.Builder.word32HexFixed",(Word32 -> Builder))
-- ("Data.ByteString.Builder.word32LE",(Word32 -> Builder))
-- ("Data.ByteString.Builder.word64BE",(Word64 -> Builder))
-- ("Data.ByteString.Builder.word64Dec",(Word64 -> Builder))
-- ("Data.ByteString.Builder.word64Hex",(Word64 -> Builder))
-- ("Data.ByteString.Builder.word64HexFixed",(Word64 -> Builder))
-- ("Data.ByteString.Builder.word64LE",(Word64 -> Builder))
-- ("Data.ByteString.Builder.word8",(Word8 -> Builder))
-- ("Data.ByteString.Builder.word8Dec",(Word8 -> Builder))
-- ("Data.ByteString.Builder.word8Hex",(Word8 -> Builder))
-- ("Data.ByteString.Builder.word8HexFixed",(Word8 -> Builder))
-- ("Data.ByteString.Builder.wordDec",(Word -> Builder))
-- ("Data.ByteString.Builder.wordHex",(Word -> Builder))
-- ("Data.ByteString.Lazy.all",(((Word8 -> Bool)) -> (ByteString -> Bool)))
-- ("Data.ByteString.Lazy.any",(((Word8 -> Bool)) -> (ByteString -> Bool)))
-- ("Data.ByteString.Lazy.append",(ByteString -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.appendFile",([Char] -> (ByteString -> IO (Unit))))
-- ("Data.ByteString.Lazy.break",(((Word8 -> Bool)) -> (ByteString -> (ByteString , ByteString))))
-- ("Data.ByteString.Lazy.concat",([ByteString] -> ByteString))
-- ("Data.ByteString.Lazy.concatMap",(((Word8 -> ByteString)) -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.cons",(Word8 -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.cons'",(Word8 -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.copy",(ByteString -> ByteString))
-- ("Data.ByteString.Lazy.count",(Word8 -> (ByteString -> Int64)))
-- ("Data.ByteString.Lazy.cycle",(ByteString -> ByteString))
-- ("Data.ByteString.Lazy.drop",(Int64 -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.dropWhile",(((Word8 -> Bool)) -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.elem",(Word8 -> (ByteString -> Bool)))
-- ("Data.ByteString.Lazy.elemIndex",(Word8 -> (ByteString -> Maybe (Int64))))
-- ("Data.ByteString.Lazy.elemIndexEnd",(Word8 -> (ByteString -> Maybe (Int64))))
-- ("Data.ByteString.Lazy.elemIndices",(Word8 -> (ByteString -> [Int64])))
-- ("Data.ByteString.Lazy.empty",ByteString)
-- ("Data.ByteString.Lazy.filter",(((Word8 -> Bool)) -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.find",(((Word8 -> Bool)) -> (ByteString -> Maybe (Word8))))
-- ("Data.ByteString.Lazy.findIndex",(((Word8 -> Bool)) -> (ByteString -> Maybe (Int64))))
-- ("Data.ByteString.Lazy.findIndices",(((Word8 -> Bool)) -> (ByteString -> [Int64])))
-- ("Data.ByteString.Lazy.foldl",<a> . (((a -> (Word8 -> a))) -> (a -> (ByteString -> a))))
-- ("Data.ByteString.Lazy.foldl'",<a> . (((a -> (Word8 -> a))) -> (a -> (ByteString -> a))))
-- ("Data.ByteString.Lazy.foldl1",(((Word8 -> (Word8 -> Word8))) -> (ByteString -> Word8)))
-- ("Data.ByteString.Lazy.foldl1'",(((Word8 -> (Word8 -> Word8))) -> (ByteString -> Word8)))
-- ("Data.ByteString.Lazy.foldlChunks",<a> . (((a -> (ByteString -> a))) -> (a -> (ByteString -> a))))
-- ("Data.ByteString.Lazy.foldr",<a> . (((Word8 -> (a -> a))) -> (a -> (ByteString -> a))))
-- ("Data.ByteString.Lazy.foldr1",(((Word8 -> (Word8 -> Word8))) -> (ByteString -> Word8)))
-- ("Data.ByteString.Lazy.foldrChunks",<a> . (((ByteString -> (a -> a))) -> (a -> (ByteString -> a))))
-- ("Data.ByteString.Lazy.fromChunks",([ByteString] -> ByteString))
-- ("Data.ByteString.Lazy.fromStrict",(ByteString -> ByteString))
-- ("Data.ByteString.Lazy.getContents",IO (ByteString))
-- ("Data.ByteString.Lazy.group",(ByteString -> [ByteString]))
-- ("Data.ByteString.Lazy.groupBy",(((Word8 -> (Word8 -> Bool))) -> (ByteString -> [ByteString])))
-- ("Data.ByteString.Lazy.hGet",(Handle -> (Int -> IO (ByteString))))
-- ("Data.ByteString.Lazy.hGetContents",(Handle -> IO (ByteString)))
-- ("Data.ByteString.Lazy.hGetNonBlocking",(Handle -> (Int -> IO (ByteString))))
-- ("Data.ByteString.Lazy.hPut",(Handle -> (ByteString -> IO (Unit))))
-- ("Data.ByteString.Lazy.hPutNonBlocking",(Handle -> (ByteString -> IO (ByteString))))
-- ("Data.ByteString.Lazy.hPutStr",(Handle -> (ByteString -> IO (Unit))))
-- ("Data.ByteString.Lazy.head",(ByteString -> Word8))
-- ("Data.ByteString.Lazy.index",(ByteString -> (Int64 -> Word8)))
-- ("Data.ByteString.Lazy.init",(ByteString -> ByteString))
-- ("Data.ByteString.Lazy.inits",(ByteString -> [ByteString]))
-- ("Data.ByteString.Lazy.interact",(((ByteString -> ByteString)) -> IO (Unit)))
-- ("Data.ByteString.Lazy.intercalate",(ByteString -> ([ByteString] -> ByteString)))
-- ("Data.ByteString.Lazy.intersperse",(Word8 -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.isPrefixOf",(ByteString -> (ByteString -> Bool)))
-- ("Data.ByteString.Lazy.isSuffixOf",(ByteString -> (ByteString -> Bool)))
-- ("Data.ByteString.Lazy.iterate",(((Word8 -> Word8)) -> (Word8 -> ByteString)))
-- ("Data.ByteString.Lazy.last",(ByteString -> Word8))
-- ("Data.ByteString.Lazy.length",(ByteString -> Int64))
-- ("Data.ByteString.Lazy.map",(((Word8 -> Word8)) -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.mapAccumL",<acc> . (((acc -> (Word8 -> (acc , Word8)))) -> (acc -> (ByteString -> (acc , ByteString)))))
-- ("Data.ByteString.Lazy.mapAccumR",<acc> . (((acc -> (Word8 -> (acc , Word8)))) -> (acc -> (ByteString -> (acc , ByteString)))))
-- ("Data.ByteString.Lazy.maximum",(ByteString -> Word8))
-- ("Data.ByteString.Lazy.minimum",(ByteString -> Word8))
-- ("Data.ByteString.Lazy.notElem",(Word8 -> (ByteString -> Bool)))
-- ("Data.ByteString.Lazy.null",(ByteString -> Bool))
-- ("Data.ByteString.Lazy.pack",([Word8] -> ByteString))
-- ("Data.ByteString.Lazy.partition",(((Word8 -> Bool)) -> (ByteString -> (ByteString , ByteString))))
-- ("Data.ByteString.Lazy.putStr",(ByteString -> IO (Unit)))
-- ("Data.ByteString.Lazy.putStrLn",(ByteString -> IO (Unit)))
-- ("Data.ByteString.Lazy.readFile",([Char] -> IO (ByteString)))
-- ("Data.ByteString.Lazy.repeat",(Word8 -> ByteString))
-- ("Data.ByteString.Lazy.replicate",(Int64 -> (Word8 -> ByteString)))
-- ("Data.ByteString.Lazy.reverse",(ByteString -> ByteString))
-- ("Data.ByteString.Lazy.scanl",(((Word8 -> (Word8 -> Word8))) -> (Word8 -> (ByteString -> ByteString))))
-- ("Data.ByteString.Lazy.singleton",(Word8 -> ByteString))
-- ("Data.ByteString.Lazy.snoc",(ByteString -> (Word8 -> ByteString)))
-- ("Data.ByteString.Lazy.span",(((Word8 -> Bool)) -> (ByteString -> (ByteString , ByteString))))
-- ("Data.ByteString.Lazy.split",(Word8 -> (ByteString -> [ByteString])))
-- ("Data.ByteString.Lazy.splitAt",(Int64 -> (ByteString -> (ByteString , ByteString))))
-- ("Data.ByteString.Lazy.splitWith",(((Word8 -> Bool)) -> (ByteString -> [ByteString])))
-- ("Data.ByteString.Lazy.stripPrefix",(ByteString -> (ByteString -> Maybe (ByteString))))
-- ("Data.ByteString.Lazy.stripSuffix",(ByteString -> (ByteString -> Maybe (ByteString))))
-- ("Data.ByteString.Lazy.tail",(ByteString -> ByteString))
-- ("Data.ByteString.Lazy.tails",(ByteString -> [ByteString]))
-- ("Data.ByteString.Lazy.take",(Int64 -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.takeWhile",(((Word8 -> Bool)) -> (ByteString -> ByteString)))
-- ("Data.ByteString.Lazy.toChunks",(ByteString -> [ByteString]))
-- ("Data.ByteString.Lazy.toStrict",(ByteString -> ByteString))
-- ("Data.ByteString.Lazy.transpose",([ByteString] -> [ByteString]))
-- ("Data.ByteString.Lazy.uncons",(ByteString -> Maybe (((Word8 , ByteString)))))
-- ("Data.ByteString.Lazy.unfoldr",<a> . (((a -> Maybe (((Word8 , a))))) -> (a -> ByteString)))
-- ("Data.ByteString.Lazy.unpack",(ByteString -> [Word8]))
-- ("Data.ByteString.Lazy.unsnoc",(ByteString -> Maybe (((ByteString , Word8)))))
-- ("Data.ByteString.Lazy.unzip",([(Word8 , Word8)] -> (ByteString , ByteString)))
-- ("Data.ByteString.Lazy.writeFile",([Char] -> (ByteString -> IO (Unit))))
-- ("Data.ByteString.Lazy.zip",(ByteString -> (ByteString -> [(Word8 , Word8)])))
-- ("Data.ByteString.Lazy.zipWith",<a> . (((Word8 -> (Word8 -> a))) -> (ByteString -> (ByteString -> [a]))))
-- ("Data.Either.Left",<b> . <a> . (a -> Either (a) (b)))
-- ("Data.Either.Right",<b> . <a> . (b -> Either (a) (b)))
-- ("Data.Either.either",<c> . <b> . <a> . (((a -> c)) -> (((b -> c)) -> (Either (a) (b) -> c))))
-- ("Data.Either.fromLeft",<b> . <a> . (a -> (Either (a) (b) -> a)))
-- ("Data.Either.fromRight",<b> . <a> . (b -> (Either (a) (b) -> b)))
-- ("Data.Either.isLeft",<b> . <a> . (Either (a) (b) -> Bool))
-- ("Data.Either.isRight",<b> . <a> . (Either (a) (b) -> Bool))
-- ("Data.Either.lefts",<b> . <a> . ([Either (a) (b)] -> [a]))
-- ("Data.Either.partitionEithers",<b> . <a> . ([Either (a) (b)] -> ([a] , [b])))
-- ("Data.Either.rights",<b> . <a> . ([Either (a) (b)] -> [b]))
-- ("Data.Function.const",<b> . <a> . (a -> (b -> a)))
-- ("Data.Function.fix",<a> . (((a -> a)) -> a))
-- ("Data.Function.flip",<c> . <b> . <a> . (((a -> (b -> c))) -> (b -> (a -> c))))
-- ("Data.Function.id",<a> . (a -> a))
-- ("Data.Function.id_0'ho'",<a> . Fun (a) (a))
-- ("Data.Function.on",<c> . <b> . <a> . (((b -> (b -> c))) -> (((a -> b)) -> (a -> (a -> c)))))
-- ("Data.List.group",<a> . (@@hplusTC@@Eq (a) -> ([a] -> [[a]])))
-- ("Data.Maybe.Just",<a> . (a -> Maybe (a)))
-- ("Data.Maybe.Nothing",<a> . Maybe (a))
-- ("Data.Maybe.catMaybes",<a> . ([Maybe (a)] -> [a]))
-- ("Data.Maybe.fromJust",<a> . (Maybe (a) -> a))
-- ("Data.Maybe.fromMaybe",<a> . (a -> (Maybe (a) -> a)))
-- ("Data.Maybe.isJust",<a> . (Maybe (a) -> Bool))
-- ("Data.Maybe.isNothing",<a> . (Maybe (a) -> Bool))
-- ("Data.Maybe.listToMaybe",<a> . ([a] -> Maybe (a)))
-- ("Data.Maybe.mapMaybe",<b> . <a> . (((a -> Maybe (b))) -> ([a] -> [b])))
-- ("Data.Maybe.maybe",<b> . <a> . (b -> (((a -> b)) -> (Maybe (a) -> b))))
-- ("Data.Maybe.maybeToList",<a> . (Maybe (a) -> [a]))
-- ("Data.Tuple.curry",<c> . <b> . <a> . ((((a , b) -> c)) -> (a -> (b -> c))))
-- ("Data.Tuple.fst",<b> . <a> . ((a , b) -> a))
-- ("Data.Tuple.snd",<b> . <a> . ((a , b) -> b))
-- ("Data.Tuple.swap",<b> . <a> . ((a , b) -> (b , a)))
-- ("Data.Tuple.uncurry",<c> . <b> . <a> . (((a -> (b -> c))) -> ((a , b) -> c)))
-- ("GHC.Char.chr",(Int -> Char))
-- ("GHC.Char.eqChar",(Char -> (Char -> Bool)))
-- ("GHC.Char.neChar",(Char -> (Char -> Bool)))
-- ("GHC.List.all",<a> . (((a -> Bool)) -> ([a] -> Bool)))
-- ("GHC.List.and",([Bool] -> Bool))
-- ("GHC.List.any",<a> . (((a -> Bool)) -> ([a] -> Bool)))
-- ("GHC.List.break",<a> . (((a -> Bool)) -> ([a] -> ([a] , [a]))))
-- ("GHC.List.concat",<a> . ([[a]] -> [a]))
-- ("GHC.List.concatMap",<b> . <a> . (((a -> [b])) -> ([a] -> [b])))
-- ("GHC.List.cycle",<a> . ([a] -> [a]))
-- ("GHC.List.drop",<a> . (Int -> ([a] -> [a])))
-- ("GHC.List.dropWhile",<a> . (((a -> Bool)) -> ([a] -> [a])))
-- ("GHC.List.elem",<a> . (@@hplusTC@@Eq (a) -> (a -> ([a] -> Bool))))
-- ("GHC.List.filter",<a> . (((a -> Bool)) -> ([a] -> [a])))
-- ("GHC.List.foldl",<b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> b))))
-- ("GHC.List.foldl'",<b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> b))))
-- ("GHC.List.foldl1",<a> . (((a -> (a -> a))) -> ([a] -> a)))
-- ("GHC.List.foldl1'",<a> . (((a -> (a -> a))) -> ([a] -> a)))
-- ("GHC.List.foldr",<b> . <a> . (((a -> (b -> b))) -> (b -> ([a] -> b))))
-- ("GHC.List.foldr1",<a> . (((a -> (a -> a))) -> ([a] -> a)))
-- ("GHC.List.head",<a> . ([a] -> a))
-- ("GHC.List.head_0'ho'",<a> . Fun (([a])) (a))
-- ("GHC.List.init",<a> . ([a] -> [a]))
-- ("GHC.List.iterate",<a> . (((a -> a)) -> (a -> [a])))
-- ("GHC.List.iterate'",<a> . (((a -> a)) -> (a -> [a])))
-- ("GHC.List.last",<a> . ([a] -> a))
-- ("GHC.List.length",<a> . ([a] -> Int))
-- ("GHC.List.lookup",<b> . <a> . (@@hplusTC@@Eq (a) -> (a -> ([(a , b)] -> Maybe (b)))))
-- ("GHC.List.map",<b> . <a> . (((a -> b)) -> ([a] -> [b])))
-- ("GHC.List.maximum",<a> . (@@hplusTC@@Ord (a) -> ([a] -> a)))
-- ("GHC.List.minimum",<a> . (@@hplusTC@@Ord (a) -> ([a] -> a)))
-- ("GHC.List.notElem",<a> . (@@hplusTC@@Eq (a) -> (a -> ([a] -> Bool))))
-- ("GHC.List.null",<a> . ([a] -> Bool))
-- ("GHC.List.or",([Bool] -> Bool))
-- ("GHC.List.product",<a> . (@@hplusTC@@Num (a) -> ([a] -> a)))
-- ("GHC.List.repeat",<a> . (a -> [a]))
-- ("GHC.List.replicate",<a> . (Int -> (a -> [a])))
-- ("GHC.List.reverse",<a> . ([a] -> [a]))
-- ("GHC.List.scanl",<b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> [b]))))
-- ("GHC.List.scanl'",<b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> [b]))))
-- ("GHC.List.scanl1",<a> . (((a -> (a -> a))) -> ([a] -> [a])))
-- ("GHC.List.scanr",<b> . <a> . (((a -> (b -> b))) -> (b -> ([a] -> [b]))))
-- ("GHC.List.scanr1",<a> . (((a -> (a -> a))) -> ([a] -> [a])))
-- ("GHC.List.span",<a> . (((a -> Bool)) -> ([a] -> ([a] , [a]))))
-- ("GHC.List.splitAt",<a> . (Int -> ([a] -> ([a] , [a]))))
-- ("GHC.List.sum",<a> . (@@hplusTC@@Num (a) -> ([a] -> a)))
-- ("GHC.List.tail",<a> . ([a] -> [a]))
-- ("GHC.List.take",<a> . (Int -> ([a] -> [a])))
-- ("GHC.List.takeWhile",<a> . (((a -> Bool)) -> ([a] -> [a])))
-- ("GHC.List.uncons",<a> . ([a] -> Maybe (((a , [a])))))
-- ("GHC.List.unzip",<b> . <a> . ([(a , b)] -> ([a] , [b])))
-- ("GHC.List.unzip3",<c> . <b> . <a> . ([((a , b) , c)] -> (([a] , [b]) , [c])))
-- ("GHC.List.zip",<b> . <a> . ([a] -> ([b] -> [(a , b)])))
-- ("GHC.List.zip3",<c> . <b> . <a> . ([a] -> ([b] -> ([c] -> [((a , b) , c)]))))
-- ("GHC.List.zipWith",<c> . <b> . <a> . (((a -> (b -> c))) -> ([a] -> ([b] -> [c]))))
-- ("GHC.List.zipWith3",<d> . <c> . <b> . <a> . (((a -> (b -> (c -> d)))) -> ([a] -> ([b] -> ([c] -> [d])))))
-- ("Nil",<a> . {[a]|_v == (Nil)})
-- ("Pair",<b> . <a> . (a -> (b -> {(a , b)|_v == (Pair x y)})))
-- ("Text.Show.show",<a> . (@@hplusTC@@Show (a) -> (a -> [Char])))
-- ("Text.Show.showChar",(Char -> ([Char] -> [Char])))
-- ("Text.Show.showList",<a> . (@@hplusTC@@Show (a) -> ([a] -> ([Char] -> [Char]))))
-- ("Text.Show.showListWith",<a> . (((a -> ([Char] -> [Char]))) -> ([a] -> ([Char] -> [Char]))))
-- ("Text.Show.showParen",(Bool -> ((([Char] -> [Char])) -> ([Char] -> [Char]))))
-- ("Text.Show.showString",([Char] -> ([Char] -> [Char])))
-- ("Text.Show.shows",<a> . (@@hplusTC@@Show (a) -> (a -> ([Char] -> [Char]))))
-- ("Text.Show.showsPrec",<a> . (@@hplusTC@@Show (a) -> (Int -> (a -> ([Char] -> [Char])))))
-- ("arg0",(a -> b))
-- ("arg0'ho'",Fun (a) (b))
-- ("arg1",c)
-- ("fst",<b> . <a> . ((a , b) -> a))
-- ("snd",<b> . <a> . ((a , b) -> b))

type TopDownSolver m = StateT CheckerState (LogicT m)

evalTopDownSolverList :: Monad m => Chan Message -> [TopDownSolver m a] -> m a
evalTopDownSolverList messageChan m = do
  observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m

-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO ()
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolverList messageChan (map helper [1..]) >> return ()
  where
    -- filters out type classes (@@type_class@@) so that numArgs can be correct when used
    -- in filterParams
    filterOutTypeClass :: [Id] -> [Id]
    filterOutTypeClass xs = filter (not . \x -> "tc" `isPrefixOf` (show x)) xs

  
    -- calls dfs at a certain depth and checks to see if there is a solution
    helper :: Int -> TopDownSolver IO RProgram
    helper quota = do
      liftIO $ printf "running dfs on %s at size %d\n" (show goal) quota

      let goalType = shape $ lastType (toMonotype goal) :: SType
      solution <- dfs env messageChan quota goalType :: TopDownSolver IO RProgram
      
      -- liftIO $ printf "solution: %s\n" (show solution)
      isChecked <- liftIO $ check' solution
      guard isChecked -- gets the first valid program

      return solution
    
    -- wrapper for `check` function
    check' :: RProgram -> IO Bool
    check' program = do
      -- printf "omg we are checking this program: %s\n" (show program)
      let blah = filterParams program
      if blah
        
        then do
          -- liftIO $ printf "program: %s\n" $ show program
          
          checkResult <- evalStateT (check env searchParams examples program goal messageChan) emptyFilterState
          case checkResult of
            Nothing  -> return False
            Just exs -> do
              out <- toOutput env program exs
              printResult $ encodeWithPrefix out
              return True
        else do
          -- liftIO $ printf "\t\tthis doesn't have all the args: %s\n" $ show program
          return False

    -- determines if the result has all the appropriate arguments
    filterParams :: RProgram -> Bool
    filterParams program = all (`isInfixOf` (show program)) $ Map.keys $ env ^. arguments

--
-- does DFS stuff, with max size of program given as a quota
--
dfs :: Environment -> Chan Message -> Int -> SType -> TopDownSolver IO RProgram
dfs env messageChan quota goalType
  | quota <= 0 = mzero
  | otherwise  = do
    -- collect all the component types (which we might use to fill the holes)
    component <- choices $ Map.toList (env ^. symbols)

    -- if goalType is an arrow type a->b, turn it into a Fun a b
    let goalType' =
          if (isFunctionType goalType)
            then shape $ toFunType $ refineTop env goalType
                  -- TODO or we can call DFS to synthesize \x -> ...
                  -- see the cartProduct test
            else goalType

    -- dfsResults <- dfs ...
    
    -- stream of components that unify with goal type
    (id, schema) <- getUnifiedComponents env messageChan component goalType' :: TopDownSolver IO (Id, SType)
    
    -- newList <- dfsResults1, getUnifiedComponents1, dfsResults2, getUnifiedComponents2
    
    -- remove 'ho' from higher order things
    -- ("(Data.Eq./=)_0'ho'",<a> . (@@hplusTC@@Eq (a) -> (a -> Fun (a) (Bool))))
    -- ("(Data.Eq./=)_1'ho'",<a> . (@@hplusTC@@Eq (a) -> Fun (a) ((Fun (a) (Bool)))))
    -- ("(Data.Eq./=)_2'ho'",<a> . Fun ((@@hplusTC@@Eq (a))) ((Fun (a) ((Fun (a) (Bool))))))
    let id' = if "'ho'" `isSuffixOf` id
          then reverse $ drop 4 $ reverse id -- hack to remove 'ho' TODO fix this later
          else id

    -- stream of solutions to the (id, schema) returned from getUnifiedComponents
    if isGround schema
      then return Program { content = PSymbol id', typeOf = refineTop env schema }
      else do
        -- collect all the argument types (the holes ?? we need to fill)
        let args = allArgTypes schema :: [SType]

        -- do basically this:
        -- dfsstuff0 <- dfs ... arg0 (quota - 1) :: RProgram
        -- dfsstuff1 <- dfs ... arg1 (quota - 1 - sizeOf dfsstuff0) :: RProgram
        -- dfsstuff2 <- dfs ... arg2 (quota - 1 - sizeOf dfsstuff0 - sizeOf dfsstuff1) :: RProgram
        -- argsFilled = [dfsstuff0, dfsstuff1, dfsstuff2]
        let func :: (Int, [RProgram]) -> SType -> TopDownSolver IO (Int, [RProgram])
            func (quota', programs) arg = do
              program <- dfs env messageChan quota' arg
              return (quota' - sizeOf program, programs ++ [program])

        (_, argsFilled) <- foldM func (quota - 1, []) args :: TopDownSolver IO (Int, [RProgram])

        return Program { content = PApp id' argsFilled, typeOf = refineTop env schema } 
      
  where
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True
    
    -- converts [a] to a Logic a
    choices :: MonadPlus m => [a] -> m a
    choices = msum . map return

    -- gets the size of a program, used for checking quota
    sizeOf :: RProgram -> Int
    sizeOf = length . words . show

--
-- Given a component (id, schema) like ("length", <a>. [a] -> Int)
--
getUnifiedComponents :: Environment -> Chan Message -> (Id, RSchema) -> SType -> TopDownSolver IO (Id, SType)
getUnifiedComponents env messageChan (id, schema) goalType = do

    freshVars <- freshType (env ^. boundTypeVars) schema

    let t1 = shape (lastType freshVars) :: SType
    let t2 = goalType :: SType

    solveTypeConstraint env t1 t2 :: TopDownSolver IO ()
    st' <- get
    
    let sub = st' ^. typeAssignment
    let checkResult = st' ^. isChecked
    -- when (show goalType == "(a -> b)") $ do
    --   (liftIO $ printf "unifying (%s) with (%s, %s), " (show goalType) (show id) (show schema))
    --   (liftIO $ printf "isChecked: %s, sub: %s\n" (show checkResult) (show sub))

    -- if it unifies, add that particular unified compoenent to state's list of components
    if (checkResult)
      then return (id, stypeSubstitute sub (shape freshVars))
      else mzero
