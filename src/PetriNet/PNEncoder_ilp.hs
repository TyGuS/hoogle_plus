{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PetriNet.PNEncoder_ilp(
     encoderInit
    , encoderSolve
    ) where

import Data.Maybe
import Data.List
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Z3.Monad hiding(Z3Env, newEnv)
import qualified Z3.Base as Z3
import Control.Monad.State
import System.CPUTime
import Text.Printf
import Data.Text (pack, unpack, replace)

import Types.Common
import Types.PetriNet
import PetriNet.PNBuilder
import Types.Encoder
import Types.Abstract
import Synquid.Util

import Numeric.Limp.Rep
import qualified Numeric.Limp.Program as ILP
import Numeric.Limp.Solvers.Cbc
import qualified Data.Map.Strict as Map
import ILP.Encoding
import Debug.Trace


encoderInit :: PetriNet -> Int -> [Id] -> [Id] -> Id -> EncodeStateILP
encoderInit net loc hoArgs inputs ret = EncodeStateILP net 
                                        (encoding ILP.Minimise 
                                                  (trace ("{places}:" ++ show a) a) 
                                                  (trace ("{transition}: " ++ show t) t) 
                                                  (trace ("{wat}" ++ show wat) wat) 
                                                  (trace ("{wta}" ++ show wta) wta) 
                                                  (trace ("{ima}" ++ show ima) ima) 
                                                  (trace ("{fma}:" ++ show fma) fma)
                                                  (trace ("{loc}:" ++ show loc) loc)
                                        ) 
                                        pids trans'
    where
        places = (HashMap.toList . pnPlaces) $ net
        trans = (HashMap.toList . pnTransitions) $ net
        a = [0..(length places - 1)]
        t = [0..(length trans - 1)]
        wat = [[mkflow net ai ti places trans | ti <- t] | ai <- a]
        wta = [[mkflowR net ai ti places trans | ai <- a] | ti <- t]
        pids = map fst places
        pids' = zip pids [0..(length pids - 1)]
        trans' = map fst trans
        ima = [fromJust $ lookup input pids' | input <- inputs]
        fma = [fromJust $ lookup ret pids']
        

-- mkflow net ai ti places trans = case HashMap.lookup ( unlines ["(","Place:" ++ (show $ fst (places !! ai)),",","Transition:" ++ (show $ fst (trans !! ti))]) $ pnFlows net of
mkflow net ai ti places trans = case HashMap.lookup (mkFlowId net (fst (places !! ai)) (fst (trans !! ti)))  $ pnFlows net of
    Just v -> flowWeight v -- fix here
    Nothing -> 0
    where
        flows = pnFlows net
mkflowR net ai ti places trans = case HashMap.lookup (mkFlowId net (fst (trans !! ti)) (fst (places !! ai)))  $ pnFlows net of
    Just v -> flowWeight v
    Nothing -> 0
    where
        flows = pnFlows net

mkFlowId :: PetriNet -> Id -> Id -> Id
mkFlowId pn from to = unlines ["(", fromLabel, ",", toLabel, ")"]
    where
    prefixLabel id | id `HashMap.member` (pnPlaces pn) = "Place:"
    prefixLabel id | id `HashMap.member` (pnTransitions pn) = "Transition:"
    prefixLabel id = error $ id ++ " is neither a place id or a transition id"

    fromLabel = (prefixLabel from) ++ from
    toLabel = (prefixLabel to) ++ to

encoderSolve :: EncodeStateILP -> IO ([(Id, Int)],EncodeStateILP)
encoderSolve state = 
    case solve $ prog state of
    Left e -> return ([], state)
    Right res@(Assignment zs _) -> return (getResult (transitions state) $ Map.filterWithKey (\k a -> a /= Z 0 && 'F' == head k) zs, state)

getResult :: [Id] -> Map.Map String (Z c) -> [(Id, Int)]
getResult trans zs =  map (\k -> (trans !! (read (head (split k)) :: Int ), read (last (split k)) :: Int)) $ Map.keys zs
  where
    split k = tail $ wordsWhen (=='_') k

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                          where (w, s'') = break p s'