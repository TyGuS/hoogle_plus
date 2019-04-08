{-# LANGUAGE FlexibleContexts #-}

module HooglePlus.CodeFormer(
      generateProgram
    , FormerState(..)
    , CodePieces
    ) where

import Types.Common
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

type Code = String
type CodePieces = Set Code
data FormerState = FormerState {
    varCounter :: Int,
    typedTerms :: HashMap String CodePieces,
    createdVars :: [Code],
    allSignatures :: [FunctionCode]
}

type CodeFormer = StateT FormerState IO

newVar :: CodeFormer Code
newVar = do
    st <- get
    let count = varCounter st
    put $ st { varCounter = count + 1 }
    return $ "x" ++ show count

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

    generateArg tArg | "->" `isInfixOf` tArg = do
        -- find the function first
        let curr = case filter ((==) tArg . funName) $ hoParams func of
                        []  -> error $ "cannot find higher order param " ++ tArg
                        h:_ -> h
        -- generate higher order argument header
        vars <- mapM (\_ -> do
                                v <- newVar
                                st <- get
                                put $ st { createdVars = v : (createdVars st)
                                         }
                                return v) (funParams curr)
        let hoHeader = "\\" ++ concat (intersperse " " vars) ++ " -> "
        -- find argument body
        tterms <- typedTerms <$> get
        oldSt <- get
        let sigsAvail = take (fromJust (elemIndex func (allSignatures oldSt))) (allSignatures oldSt)
        bodies <- generateProgram sigsAvail (funParams curr) vars (head (funReturn curr)) False
        put oldSt
        case Set.toList bodies of
            [] -> return []
            _  -> return $ map (withParen . (++) hoHeader) (Set.toList bodies)
    generateArg tArg | otherwise = do
        tterms <- typedTerms <$> get
        return $ Set.toList $ HashMap.lookupDefault Set.empty tArg tterms

-- | generate the program from the signatures appeared in selected transitions
-- these signatures are sorted by their timestamps,
-- i.e. they may only use symbols appearing before them
generateProgram :: [FunctionCode] -> [Id] -> [Id] -> Id -> Bool -> CodeFormer CodePieces
generateProgram signatures inputs argNames ret isFinal = do
    -- prepare scalar variables
    st <- get
    put $ st { varCounter = 0
             , allSignatures = signatures
             }
    mapM_ (uncurry addTypedArg) $ zip inputs argNames
    -- mapM_ addHOParams signatures
    -- reset varCounter before filling sketch
    st <- get
    put $ st { varCounter = 0 }
    mapM_ applyFunction signatures
    codePieces <- HashMap.lookupDefault Set.empty ret . typedTerms <$> get
    let vars = []
    return $ Set.filter (includeAllSymbols vars) codePieces
  where
    addTypedArg input argName = do
        st <- get
        let tterms = typedTerms st
        let newTerms = HashMap.insertWith Set.union input (Set.singleton argName) tterms
        put $ st { typedTerms = newTerms }

    -- ignore the nested higher order arguments
    addHOParams func = do
        tterms <- typedTerms <$> get
        mapM_ (mapM_ addHOParam . funParams) $ hoParams func

    addHOParam fparam = do
        argName <- newVar
        addTypedArg fparam argName

    includeAllSymbols vars code =
        let fold_fn f (b, c) = if b then includeSymbol c f else (b, c)
            base             = (True, code)
            funcNames        = map funName signatures
            (res, c)         = foldr fold_fn base (argNames ++ if isFinal then funcNames else []) -- vars exists after slash and the function body, at least twice
            -- eachOnce         = foldr (\n acc -> isInfixOf n c || acc) False (if isFinal then funcNames else []) -- each function should be used only once according to our design of petri net
        in res -- && (not eachOnce)

    includeSymbol code fname | fname `isInfixOf` code =
        (True, Text.unpack $ replaceOne (Text.pack fname) Text.empty (Text.pack code))
    includeSymbol _    _     | otherwise              = (False, [])

    replaceOne :: Text -> Text -> Text -> Text
    replaceOne pattern substitution text
      | Text.null back = text    -- pattern doesn't occur
      | otherwise = Text.concat [front, substitution, Text.drop (Text.length pattern) back]
        where
          (front, back) = Text.breakOn pattern text
