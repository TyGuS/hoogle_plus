module Examples.Utils where

import           Data.Functor                   ( (<&>) )
import           Data.List.Extra                ( dropEnd )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Debugger                       ( showTerm )
import qualified EnumSet                       as ES
import           Exception                      ( SomeException
                                                , catch
                                                , gtry
                                                )
import           GHC                     hiding ( Id )
import           GHC.LanguageExtensions.Type    ( Extension
                                                  ( ExtendedDefaultRules
                                                  , FlexibleContexts
                                                  )
                                                )
import           GHC.Paths                      ( libdir )
import qualified Language.Haskell.Interpreter  as LHI
import           Outputable                     ( showSDocUnsafe )
import           System.Timeout                 ( timeout )
import           Text.Printf                    ( printf )

import           HooglePlus.FilterTest          ( runInterpreter' )
import           Types.Common
import           Types.Environment
import           Types.Filtering                ( defaultDepth
                                                , defaultInterpreterTimeoutMicro
                                                , defaultTimeoutMicro
                                                , frameworkModules
                                                )
import           Types.Type
import           Types.TypeChecker

askGhc :: [Id] -> Ghc a -> IO a
askGhc mdls f = do
  mbResult <- timeout (10 ^ 6) $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = dflags
          { generalFlags   = ES.delete Opt_OmitYields (generalFlags dflags)
          , extensionFlags = ES.insert FlexibleContexts (extensionFlags dflags)
          }
    setSessionDynFlags dflags'
    prepareModules ("Test.ChasingBottoms" : "Prelude" : mdls) >>= setContext
    f
  case mbResult of
    Just r  -> return r
    Nothing -> error "timeout"
 where
  prepareModules mdls = do
    let imports = map (printf "import %s") mdls
    decls <- mapM parseImportDecl imports
    return (map IIDecl decls)

runStmt :: [Id] -> String -> IO (Either GHCError String)
runStmt mdls prog = do
  catch
    (askGhc mdls $ do
    -- allow type defaulting during execution
      dflags <- getSessionDynFlags
      let dflags' = dflags
            { extensionFlags = ES.insert ExtendedDefaultRules
                                         (extensionFlags dflags)
            }
      setSessionDynFlags dflags'
      result <- execStmt prog execOptions
      case result of
        ExecComplete r _ -> case r of
          Left  e  -> return (Left (show e))
          Right ns -> getExecValue ns
        ExecBreak{} -> return (Left "error, break")
    )
    (\(e :: SomeException) -> return (Left $ show e))
 where
  getExecValue (n : ns) = do
    mty <- lookupName n
    case mty of
      Just (AnId aid) -> do
        t <- gtry $ obtainTermFromId maxBound True aid
        case t of
          Right term ->
            showTerm term <&> (Right . dropEnd 1 . drop 1 . showSDocUnsafe)
          Left (exn :: SomeException) -> return (Left $ show exn)
      _ -> return (Left "Unknown error")
  getExecValue [] = return (Left "Empty result list")

skipTyclass :: TypeSkeleton -> TypeSkeleton
skipTyclass (FunctionT x (DatatypeT name args) tRes)
  | tyclassPrefix `Text.isPrefixOf` name = skipTyclass tRes
skipTyclass t = t

seqChars :: [Id]
seqChars = map Text.singleton ['a' .. 'z']

integerToInt :: TypeSkeleton -> TypeSkeleton
integerToInt (DatatypeT dt args)
  | dt == "Integer" = DatatypeT "Int" (map integerToInt args)
  | otherwise       = DatatypeT dt (map integerToInt args)
integerToInt (FunctionT x tArg tRes) =
  FunctionT x (integerToInt tArg) (integerToInt tRes)
integerToInt t = t

wrapParens :: String -> String
wrapParens = printf "(%s)"

-- assume the types passed into the method already have fresh variable names
checkTypes
  :: Environment
  -> [Id]
  -> TypeSkeleton
  -> TypeSkeleton
  -> Maybe TypeSubstitution
checkTypes env tmpBound r1 r2 =
  let bvs = tmpBound ++ getBoundTypeVars env
  in  solveTypeConstraint bvs
                          Map.empty
                          (SubtypeOf (skipTyclass r1) (skipTyclass r2))

mkPolyType :: TypeSkeleton -> SchemaSkeleton
mkPolyType t =
  let tvars    = Set.toList $ typeVarsOf t
      freeVars = filter ((==) wildcardPrefix . Text.take 1) tvars
  in  foldr ForallT (Monotype t) freeVars
