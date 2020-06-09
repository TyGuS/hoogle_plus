module Datalog.DataTypes where

data Constraint a =  
class DatalogConstraint a where
    repr :: Constraint a -> String
