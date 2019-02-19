{-# LANGUAGE FlexibleContexts #-}

module HooglePlus.CodeFormer(
      generateProgram
    , FormerState(..)
    , CodePieces
    ) where

import PetriNet.PNBuilder
import Synquid.Util

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text

type Code = String
type CodePieces = Set Code
data FormerState = FormerState {
    varCounter :: Int,
    typedTerms :: HashMap String CodePieces
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
    fname = removeLast '_' $ funName func

    generateArgs codeSoFar [] = return codeSoFar
    generateArgs codeSoFar (tArg:tArgs) = do
        args <- generateArg tArg
        let partialApplication = [f ++ " " ++ a | f <- codeSoFar, a <- args]
        case partialApplication of
            []          -> return []
            codePieces  -> generateArgs codePieces tArgs

    generateArg tArg | head tArg == 'f' = do
        -- find the function first
        let curr = case filter ((==) tArg . funName) $ hoParams func of
                        []  -> error $ "cannot find higher order param " ++ tArg
                        h:_ -> h
        -- generate higher order argument header
        vars <- mapM (\_ -> newVar) (funParams curr)
        let hoHeader = "\\" ++ concat (intersperse " " vars) ++ "-> "
        -- find argument body
        tterms <- typedTerms <$> get
        let bodies = Set.toList $ HashMap.lookupDefault Set.empty (head (funReturn curr)) tterms
        case bodies of
            [] -> return []
            _  -> return $ map (withParen . (++) hoHeader) bodies
    generateArg tArg | otherwise = do
        tterms <- typedTerms <$> get
        return $ Set.toList $ HashMap.lookupDefault Set.empty tArg tterms

-- | generate the program from the signatures appeared in selected transitions
-- these signatures are sorted by their timestamps, 
-- i.e. they may only use symbols appearing before them
generateProgram :: [FunctionCode] -> [Id] -> [Id] -> CodeFormer CodePieces
generateProgram signatures inputs argNames = do
    -- prepare scalar variables
    st <- get
    put $ st { varCounter = 0 }
    mapM_ (uncurry addTypedArg) $ zip inputs argNames
    mapM_ addHOParams signatures
    -- reset varCounter before filling sketch
    st <- get
    put $ st { varCounter = 0 }
    mapM_ applyFunction $ init signatures
    codePieces <- applyFunction $ last signatures
    return $ Set.filter includeAllSymbols codePieces
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

    includeAllSymbols code = 
        let fold_fn f (b, c) = if b then includeSymbol c f else (b, c)
            base             = (True, code)
            funcNames        = map (removeLast '_' . funName) signatures
            (res, c)         = foldr fold_fn base (argNames ++ funcNames)
            eachOnce         = foldr (\n acc -> isInfixOf n c || acc) False funcNames
        in res && (not eachOnce)

    includeSymbol code fname | fname `isInfixOf` code = 
        (True, Text.unpack $ replaceOne (Text.pack fname) Text.empty (Text.pack code))
    includeSymbol _    _     | otherwise              = (False, [])

    replaceOne :: Text -> Text -> Text -> Text
    replaceOne pattern substitution text
      | Text.null back = text    -- pattern doesn't occur
      | otherwise = Text.concat [front, substitution, Text.drop (Text.length pattern) back] 
        where
          (front, back) = Text.breakOn pattern text
