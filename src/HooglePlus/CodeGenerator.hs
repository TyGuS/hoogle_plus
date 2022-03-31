module HooglePlus.CodeGenerator
  ( runCodeGenerator
  , CodePieces
  ) where

import           Control.Monad.State            ( MonadState(get, put)
                                                , State
                                                , evalState
                                                , gets
                                                )
import           Data.List                      ( isSubsequenceOf
                                                , sort
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           Types.Common
import           Types.Encoder
import           Types.Program
import           Types.Type

type Code = TProgram
type CodePieces = Set Code

data GeneratorState = GeneratorState
  { typedTerms    :: Map TypeSkeleton CodePieces
  , allSignatures :: [EncodedFunction]
  }

type CodeGenerator = State GeneratorState

applyFunction :: EncodedFunction -> CodeGenerator ()
applyFunction func = do
  args <- generateArgs (funParams func)
  let progs = Set.fromList $ map (untyped . PApp fname) args
  st <- get
  let tterms   = typedTerms st
  let newTerms = Map.insertWith Set.union (head (funReturn func)) progs tterms
  put $ st { typedTerms = newTerms }
 where
  fname = funName func

  generateArgs :: [TypeSkeleton] -> CodeGenerator [[TProgram]]
  generateArgs tArgs = sequence <$> mapM termsOfType tArgs

-- | generate the program from the signatures appeared in selected transitions
-- these signatures are sorted by their timestamps,
-- i.e. they may only use symbols appearing before them
generateProgram
  :: [EncodedFunction]  -- signatures used to generate program
  -> [TypeSkeleton]  -- argument types in the query
  -> [Id]            -- argument names
  -> [TypeSkeleton]  -- return types
  -> Bool            -- relevancy toggle
  -> CodeGenerator CodePieces
generateProgram signatures inputs argNames rets disrel = do
    -- prepare scalar variables
  st <- get
  put $ st { allSignatures = signatures }
  mapM_ (uncurry addTypedArg) $ zip inputs argNames
  mapM_ applyFunction signatures
  termLib <- gets typedTerms
  terms   <- mapM termsOfType rets
  return $ Set.fromList $ filter includeAllSymbols (concat terms)
 where
  includeAllSymbols :: TProgram -> Bool
  includeAllSymbols prog =
    let funcNames = map funName signatures
    in  sort (allSymbolsIn prog) `isSubsequenceOf` sort funcNames

runCodeGenerator
  :: [EncodedFunction]  -- signatures used to generate program
  -> [TypeSkeleton]  -- argument types in the query
  -> [Id]            -- argument names
  -> [TypeSkeleton]  -- return types
  -> Bool            -- relevancy toggle
  -> CodePieces
runCodeGenerator signatures inputs argNames rets disrel = evalState
  (generateProgram signatures inputs argNames rets disrel)
  (GeneratorState Map.empty [])

-------- Internal Utils

addNewTerms :: TypeSkeleton -> CodePieces -> CodeGenerator ()
addNewTerms typ terms = do
  st <- get
  let tterms   = typedTerms st
  let newTerms = Map.insertWith Set.union typ terms tterms
  put $ st { typedTerms = newTerms }

addTypedArg :: TypeSkeleton -> Id -> CodeGenerator ()
addTypedArg input argName =
  addNewTerms input (Set.singleton $ untyped $ PSymbol argName)

termsOfType :: TypeSkeleton -> CodeGenerator [TProgram]
termsOfType typ = do
  tterms <- gets typedTerms
  case Map.lookup typ tterms of
    Nothing -> error $ "termsOfType: cannot find terms with type " ++ show typ
    Just terms -> return $ Set.toList terms
