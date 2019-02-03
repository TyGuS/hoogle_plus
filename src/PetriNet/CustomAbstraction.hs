module PetriNet.CustomAbstraction where


import qualified Data.Set as Set

import PetriNet.AbstractType

-- { List[Char],
-- Int,
-- Tuple[List[Char], List[Char]],
-- List[Tuple[List[Char], List[Char]]],
-- List[List[Char]],
-- Double,
-- List[Double],
-- Integer,
-- List[Integer],
-- List[List[Double]],
-- Char,
-- Bool,
-- List[Tuple[Integer, List[Char]]], Tuple[Int, List[Char]]
-- }
abstr = abstr2_3

abstr4 = [
    AExclusion ( Set.fromList
        [ "Bool"
        , "Int"
        , "List"
        ]
        )
    , ADatatypeT "Bool"[]
    , ADatatypeT "Int"[]
    , ADatatypeT "List"[ ADatatypeT "Int" [] ]
    , ADatatypeT "List"[ AExclusion (Set.fromList ["Int"]) ]
    ]

abstr5 = [
    AExclusion ( Set.fromList
        [ "Bool"
        , "Int"
        , "Char"
        , "List"
        ]
        )
    , ADatatypeT "Bool"[]
    , ADatatypeT "Char"[]
    , ADatatypeT "Int"[]
    , ADatatypeT "List"[ ADatatypeT "Int" [] ]
    , ADatatypeT "List"[ AExclusion (Set.fromList ["Int"]) ]
    ]

abstr6 = [
    AExclusion ( Set.fromList
        [ "Bool"
        , "Int"
        , "Char"
        , "Double"
        , "List"
        ]
        )
    , ADatatypeT "Bool"[]
    , ADatatypeT "Int"[]
    , ADatatypeT "Char"[]
    , ADatatypeT "Double"[]
    , ADatatypeT "Int"[]
    , ADatatypeT "List"[ ADatatypeT "Int" [] ]
    , ADatatypeT "List"[ AExclusion (Set.fromList ["Int"]) ]
    ]

abstr7 = [
    AExclusion ( Set.fromList
        [ "Bool"
        , "Int"
        , "Integer"
        , "Double"
        , "Char"
        , "List"
        ]
        )
    , ADatatypeT "Bool"[]
    , ADatatypeT "Int"[]
    , ADatatypeT "Integer"[]
    , ADatatypeT "Double"[]
    , ADatatypeT "Char"[]
    , ADatatypeT "List"[ ADatatypeT "Int" [] ]
    , ADatatypeT "List"[ AExclusion (Set.fromList ["Int"]) ]
    ]

-- {
-- List[Char],
-- Int,
-- Integer,
-- List[Tuple[List[Char], List[Char]]],
-- Tuple[List[Char],  List[Char]], Word8, List[Word8], List[List[Char]], List[Integer], ByteString }

abstr2_3 = [
    AExclusion ( Set.fromList
        [ "List"
        ]
        )
    , ADatatypeT "List" [ AExclusion (Set.fromList ["Char"])]
    , ADatatypeT "List"[ ADatatypeT "Char" []]
    ]

abstr2_4 = [
    AExclusion ( Set.fromList
        [ "List",
         "Int"
        ]
        )
    , ADatatypeT "List" [ AExclusion (Set.fromList ["Char"])]
    , ADatatypeT "List"[ ADatatypeT "Char" []]
    , ADatatypeT "Int" []
    ]

abstr2_5 = [
    AExclusion ( Set.fromList
        [ "List",
         "Int",
         "Integer"
        ]
        )
    , ADatatypeT "List" [ AExclusion (Set.fromList ["Char"])]
    , ADatatypeT "List"[ ADatatypeT "Char" []]
    , ADatatypeT "Int" []
    , ADatatypeT "Integer" []
    ]

abstr2_6 = [
    AExclusion ( Set.fromList
        [ "List",
         "Int",
         "Integer",
         "Word8"
        ]
        )
    , ADatatypeT "List" [ AExclusion (Set.fromList ["Char"])]
    , ADatatypeT "List"[ ADatatypeT "Char" []]
    , ADatatypeT "Int" []
    , ADatatypeT "Integer" []
    , ADatatypeT "Word8" []
    ]

abstr2_7 = [
    AExclusion ( Set.fromList
        [ "List"
        , "Int"
        , "Pair"
        ]
        )
    , ADatatypeT "List" [ AExclusion (Set.fromList ["Char"])]
    , ADatatypeT "List" [ ADatatypeT "Char" [] ]
    , ADatatypeT "Int"[]
    , ADatatypeT "Pair"[ AExclusion (Set.fromList ["List"])]
    , ADatatypeT "Pair"[ ADatatypeT "List" [ AExclusion (Set.fromList ["List"])]]
    , ADatatypeT "Pair"[ ADatatypeT "List" [ADatatypeT "Char" []]]
    ]
