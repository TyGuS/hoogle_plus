module Encoder.ConstraintEncoder where

import Types.Common
import Types.Abstract
import Types.Experiments

import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)

data FunctionCode = FunctionCode {
    funName   :: String,  -- function name
    hoParams  :: [FunctionCode],
    funParams :: [AbstractSkeleton], -- function parameter types and their count
    funReturn :: [AbstractSkeleton]   -- function return type
}

instance Eq FunctionCode where
  fc1 == fc2 = let
    areEq arg = on (==) arg fc1 fc2
    in areEq hoParams && areEq funParams && areEq funReturn

instance Ord FunctionCode where
  compare fc1 fc2 = let
    thenCmp EQ       ordering = ordering
    thenCmp ordering _        = ordering
    cmp arg = on compare arg fc1 fc2
    in foldr1 thenCmp [cmp hoParams, cmp funParams, cmp funReturn]

type TransMap = HashMap AbstractSkeleton (Set Id)

class ConstraintEncoder s where
    encoderInit     :: s
                    -> Int
                    -> [AbstractSkeleton]
                    -> [AbstractSkeleton]
                    -> [FunctionCode]
                    -> IO s
    encoderInc      :: [FunctionCode] 
                    -> [AbstractSkeleton] 
                    -> [AbstractSkeleton]
                    -> s
                    -> IO s
    encoderRefine   :: SplitInfo 
                    -> [AbstractSkeleton] 
                    -> [AbstractSkeleton] 
                    -> [FunctionCode]
                    -> s
                    -> IO s
    encoderSolve    :: s -> IO ([Id], s)

    emptyEncoder    :: s
    getTy2tr        :: s -> TransMap
    setTy2tr        :: TransMap -> s -> s
    modifyTy2tr     :: (TransMap -> TransMap) -> s -> s
    setPrevChecked  :: Bool -> s -> s
    modifyMusters   :: (HashMap Id [Id] -> HashMap Id [Id]) -> s -> s
    setParams       :: SearchParams -> s -> s
    {-# MINIMAL encoderInit, encoderInc, encoderRefine, encoderSolve,
                emptyEncoder, getTy2tr, setTy2tr, modifyTy2tr, setPrevChecked, modifyMusters, setParams #-}