{-# LANGUAGE FlexibleContexts #-}

module HooglePlus.CodeFormer(
      generateProgram
    , FormerState(..)
    , CodePieces
    ) where

import Types.Common
import Types.Encoder
import Types.Abstract
import Synquid.Util

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe
import Data.List.Split

type Code = String
type CodePieces = Set Code
data FormerState = FormerState {
    typedTerms :: HashMap AbstractSkeleton CodePieces,
    allSignatures :: [FunctionCode]
}

type CodeFormer = StateT FormerState IO

withParen :: Code -> Code
withParen c = "(" ++ c ++ ")"

applyFunction :: FunctionCode -> CodeFormer CodePieces
applyFunction func = do
    args <- generateArgs [""] (funParams func)
    let res = Set.fromList $ map (\a -> withParen $ fname ++ a) args
    -- update typedTerms
    st <- get
    let tterms = typedTerms st
    let newTerms = HashMap.insertWith Set.union (head (funReturn func)) res tterms
    put $ st { typedTerms = newTerms }
    return res
  where
    fname = funName func

    generateArgs codeSoFar [] = return codeSoFar
    generateArgs codeSoFar (tArg:tArgs) = do
        args <- generateArg tArg
        let partialApplication = [f ++ " " ++ a | f <- codeSoFar, a <- args]
        case partialApplication of
            []          -> return []
            codePieces  -> generateArgs codePieces tArgs

    generateArg tArg = do
        tterms <- gets typedTerms
        return $ Set.toList $ HashMap.lookupDefault Set.empty tArg tterms

-- | generate the program from the signatures appeared in selected transitions
-- these signatures are sorted by their timestamps,
-- i.e. they may only use symbols appearing before them
generateProgram :: [FunctionCode]      -- signatures used to generate program
                -> [AbstractSkeleton]  -- argument types in the query
                -> [Id]                -- argument names
                -> [AbstractSkeleton]  -- return types
                -> Bool                -- relevancy toggle
                -> CodeFormer CodePieces
generateProgram signatures inputs argNames rets disrel = do
    -- prepare scalar variables
    st <- get
    put $ st { allSignatures = signatures }
    mapM_ (uncurry addTypedArg) $ zip inputs argNames
    mapM_ applyFunction signatures
    termLib <- gets typedTerms
    let codePieces = map (\ret -> HashMap.lookupDefault Set.empty ret termLib) rets
    let vars = []
    return $ Set.filter (includeAllSymbols vars . splitOneOf " ()") (Set.unions codePieces)
  where
    addTypedArg input argName = do
        st <- get
        let tterms = typedTerms st
        let newTerms = HashMap.insertWith Set.union input (Set.singleton argName) tterms
        put $ st { typedTerms = newTerms }

    includeAllSymbols vars code =
        let fold_fn f (b, c) = if b && f `elem` c then (True, f `delete` c) else (False, c)
            base             = (True, code)
            funcNames        = map funName signatures
            (res, c)         = foldr fold_fn base (funcNames ++ if disrel then [] else argNames)
        in res
