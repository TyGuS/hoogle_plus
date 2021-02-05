{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, LambdaCase #-}
module InternalTypeGen where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (liftM2)
import Control.Monad.Logic (liftM)
import Data.Char (ord)
import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data(..))
import Data.List (isInfixOf, elemIndex, nub, drop, reverse, intersect, intercalate)
import Text.Printf (printf)
import qualified Test.ChasingBottoms as CB
import qualified Test.QuickCheck as QC

defaultMaxOutputLength    = 50         :: CB.Nat
defaultTimeoutMicro       = 400         :: Int
defaultIntRange           = [-2..10]    :: [Int]
defaultCharRange          = ['a'..'d']  :: [Char]
defaultFuncSpecialSize    = 4           :: Int
defaultTestArgs           = QC.stdArgs {QC.chatty = False, QC.maxDiscardRatio = 1, QC.maxSuccess = 100, QC.maxSize = 7} :: QC.Args

instance Eq a => Eq (CB.Result a) where
  (CB.Value a) == (CB.Value b) = a == b
  CB.NonTermination == CB.NonTermination = True
  (CB.Exception _) == (CB.Exception _) = True
  _ == _ = False

instance Ord a => Ord (CB.Result a) where
  (CB.Value a) `compare` (CB.Value b) = a `compare` b
  (CB.Value _) `compare` _ = GT
  (CB.Exception _) `compare` (CB.Exception _) = EQ
  (CB.Exception _) `compare` (CB.Value _) = LT
  (CB.Exception _) `compare` _ = GT
  (CB.NonTermination) `compare` (CB.NonTermination) = EQ
  (CB.NonTermination) `compare` _ = LT

isFailedResult :: CB.Result String -> Bool
isFailedResult result = case result of
  CB.NonTermination -> True
  CB.Exception _ -> True
  CB.Value a | "_|_" `isInfixOf` a -> True
  CB.Value a | "Exception" `isInfixOf` a -> True
  _ -> False

showCBResult :: CB.Result String -> String
showCBResult = \case
                  CB.Value a | "_|_" `isInfixOf` a -> "bottom"
                  CB.Value a -> a
                  CB.NonTermination -> "diverge"
                  CB.Exception ex -> show ex

anyDuplicate :: Ord a => [a] -> Bool
anyDuplicate [x, y] = x == y
anyDuplicate xs = length (nubOrd xs) /= length xs

labelEvaluation :: (Data a, ShowConstr a, QC.Testable prop) => [String] -> [String] -> [a] -> ([CB.Result String] -> prop) -> IO QC.Property
labelEvaluation inputs inputConstrs values prop = do
    outputs <- map splitResult <$> mapM (evaluateValue defaultTimeoutMicro) values
    
    let examples = map (\(a, b) -> InternalExample inputs inputConstrs (showCBResult a) (showCBResult b)) outputs
    return $ QC.label (show examples) (prop $ map fst outputs)
  where
    evaluateValue :: (Data a, ShowConstr a) => Int -> a -> IO (CB.Result (String, String))
    evaluateValue timeInMicro x = CB.timeOutMicro timeInMicro $ liftM2 (,) (t x) (s x)
      where
        t = evaluate . force . CB.approxShow defaultMaxOutputLength
        s = evaluate . CB.approxShow defaultMaxOutputLength . showConstr

    splitResult :: CB.Result (String, String) -> (CB.Result String, CB.Result String)
    splitResult = \case
      CB.Value (a, b) -> (CB.Value a, CB.Value b)
      CB.NonTermination -> (CB.NonTermination, CB.NonTermination)
      CB.Exception ex -> (CB.Exception ex, CB.Exception ex)

data InternalExample = InternalExample {
    inputs :: [String],
    inputConstrs :: [String],
    output :: String,
    outputConstr :: String
} deriving(Eq, Show)

-- * Custom Datatype for Range Restriction
newtype  MyInt = MyIntValue Int deriving (Eq, Data)
instance Ord              MyInt where compare (MyIntValue l) (MyIntValue r) = compare l r      
instance Show             MyInt where show (MyIntValue v) = show v
instance QC.Arbitrary     MyInt where arbitrary = QC.elements (map MyIntValue defaultIntRange)
instance QC.CoArbitrary   MyInt where coarbitrary (MyIntValue v) = QC.coarbitraryIntegral v

newtype  MyChar = MyCharValue Char deriving (Eq, Data)
instance Ord              MyChar where compare (MyCharValue l) (MyCharValue r) = compare l r
instance Show             MyChar where show (MyCharValue v) = show v
instance QC.Arbitrary     MyChar where arbitrary = QC.elements (map MyCharValue defaultCharRange)
instance QC.CoArbitrary   MyChar where coarbitrary (MyCharValue v) = QC.coarbitrary $ ord v

data     MyFun a b = Generated (a -> b) | Expression String (a -> b)
instance (QC.Arbitrary a, QC.CoArbitrary b)                       => QC.CoArbitrary (MyFun a b)   where coarbitrary = \case Generated f -> QC.coarbitrary f; Expression _ f -> QC.coarbitrary f
instance Show a                                                   => Show (MyFun a b)             where show = \case Expression str _ -> "(" ++ str ++ ")"; Generated f -> "<Generated>"
instance {-# OVERLAPPABLE #-} (QC.CoArbitrary a, QC.Arbitrary b)  => QC.Arbitrary (MyFun a b)     where arbitrary = liftM Generated QC.arbitrary

newtype  Box a            =   BoxValue a              deriving (Eq, Data)
instance Ord a            =>  Ord (Box a)             where compare (BoxValue l) (BoxValue r) = compare l r
instance Show a           =>  Show (Box a)            where show (BoxValue v) = show v
instance QC.Arbitrary a   =>  QC.Arbitrary (Box a)    where arbitrary = fmap BoxValue QC.arbitrary
instance QC.CoArbitrary a =>  QC.CoArbitrary (Box a)  where coarbitrary (BoxValue v) = QC.coarbitrary v
        
-- * Custom Datatype Conversion
class    Unwrappable a b                                                            where unwrap :: a -> b; wrap :: b -> a
instance Unwrappable a b => Unwrappable (Box a) b                                   where unwrap = unwrap . (\(BoxValue v) -> v); wrap = BoxValue . wrap
instance Unwrappable MyInt Int                                                      where unwrap (MyIntValue v) = v; wrap = MyIntValue
instance Unwrappable MyChar Char                                                    where unwrap (MyCharValue v) = v; wrap = MyCharValue
instance (Unwrappable a c, Unwrappable b d)   => Unwrappable (a -> b)    (c -> d)   where unwrap f = \x -> unwrap $ f $ wrap x; wrap f = \x -> wrap $ f $ unwrap x
instance (Unwrappable a c, Unwrappable b d)   => Unwrappable (MyFun a b) (c -> d)   where unwrap (Generated f) = unwrap f; unwrap (Expression _ f) = unwrap f; wrap f = Generated (wrap f)

instance                                         Unwrappable a a                    where unwrap = id; wrap = id
instance {-# OVERLAPPING #-} Unwrappable a b  => Unwrappable [a] [b]                where unwrap = fmap unwrap; wrap = fmap wrap
instance {-# OVERLAPPING #-} Unwrappable a b  => Unwrappable (Maybe a) (Maybe b)    where unwrap = fmap unwrap; wrap = fmap wrap
instance (Unwrappable a c, Unwrappable b d)   => Unwrappable (a, b) (c, d)          where unwrap (x, y) = (unwrap x, unwrap y); wrap (x, y) = (wrap x, wrap y)

instance (Unwrappable a c, Unwrappable b d)   => Unwrappable (Either a b) (Either c d) where
  wrap    = \case Left v -> Left $ wrap v;    Right v -> Right $ wrap v
  unwrap  = \case Left v -> Left $ unwrap v;  Right v -> Right $ unwrap v

class                                       ShowConstr a            where showConstr :: a -> String
instance                                    ShowConstr Int          where showConstr x = show $ x `compare` 0
instance                                    ShowConstr Bool         where showConstr = show
instance                                    ShowConstr Char         where showConstr = const "_"
instance                                    ShowConstr MyInt        where showConstr = const "_"
instance                                    ShowConstr MyChar       where showConstr = const "_"
instance                                    ShowConstr (Box a)      where showConstr = const "_"
instance                                    ShowConstr (a -> b)     where showConstr = const "<func>"
instance                                    ShowConstr (MyFun a b)  where showConstr = \case Generated _ -> "<func>"; Expression n _ -> n
instance  ShowConstr a                  =>  ShowConstr (Maybe a)    where showConstr = \case Just x -> "Just$" ++ showConstr x; Nothing -> "Nothing"
instance  (ShowConstr a, ShowConstr b)  =>  ShowConstr (a, b)       where showConstr (x, y) = printf "(%s,%s)" (showConstr x) (showConstr y)
instance  (ShowConstr a)                =>  ShowConstr [a]          where showConstr xs = intercalate "," $ map showConstr xs
instance  (ShowConstr a, ShowConstr b)  =>  ShowConstr (Either a b) where showConstr = \case Left x -> "Left$" ++ showConstr x; Right y -> "Right$" ++ showConstr y