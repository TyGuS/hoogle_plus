module Database.Dataset
  ( hplusComponents
  , hplusHigherOrders
  , includedModules
  , resolver
  ) where

import           Data.Text                      ( Text )

import           Types.Resolver
import           Types.Type


hplusComponents :: [(Text, SchemaSkeleton)]
hplusComponents =
  [ ( "(Data.Bool.&&)"
    , Monotype
      (FunctionT
        "arg0"
        (DatatypeT "Bool" [])
        (FunctionT "arg1" (DatatypeT "Bool" []) (DatatypeT "Bool" []))
      )
    )
  , ( "(Data.Bool.||)"
    , Monotype
      (FunctionT
        "arg2"
        (DatatypeT "Bool" [])
        (FunctionT "arg3" (DatatypeT "Bool" []) (DatatypeT "Bool" []))
      )
    )
  , ( "(Data.Eq./=)"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg305"
          (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
          (FunctionT "arg303"
                     (TypeVarT "a")
                     (FunctionT "arg304" (TypeVarT "a") (DatatypeT "Bool" []))
          )
        )
      )
    )
  , ( "(Data.Eq.==)"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg302"
          (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
          (FunctionT "arg300"
                     (TypeVarT "a")
                     (FunctionT "arg301" (TypeVarT "a") (DatatypeT "Bool" []))
          )
        )
      )
    )
  , ( "(Data.Function.$)"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "arg319"
                     (FunctionT "arg321" (TypeVarT "a") (TypeVarT "b"))
                     (FunctionT "arg320" (TypeVarT "a") (TypeVarT "b"))
          )
        )
      )
    )
  , ( "(Data.Function..)"
    , ForallT
      "a"
      (ForallT
        "b"
        (ForallT
          "c"
          (Monotype
            (FunctionT
              "arg309"
              (FunctionT "arg313" (TypeVarT "b") (TypeVarT "c"))
              (FunctionT "arg310"
                         (FunctionT "arg312" (TypeVarT "a") (TypeVarT "b"))
                         (FunctionT "arg311" (TypeVarT "a") (TypeVarT "c"))
              )
            )
          )
        )
      )
    )
  , ( "(GHC.List.!!)"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg385"
                   (DatatypeT "List" [TypeVarT "a"])
                   (FunctionT "arg386" (DatatypeT "Int" []) (TypeVarT "a"))
        )
      )
    )
  , ( "(GHC.List.++)"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg372"
          (DatatypeT "List" [TypeVarT "a"])
          (FunctionT "arg373"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "List" [TypeVarT "a"])
          )
        )
      )
    )
  , ( "(Prelude.<)"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg513"
          (DatatypeT "@@hplusTC@@Ord" [TypeVarT "a"])
          (FunctionT "arg511"
                     (TypeVarT "a")
                     (FunctionT "arg512" (TypeVarT "a") (DatatypeT "Bool" []))
          )
        )
      )
    )
  , ( "(Prelude.<=)"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg516"
          (DatatypeT "@@hplusTC@@Ord" [TypeVarT "a"])
          (FunctionT "arg514"
                     (TypeVarT "a")
                     (FunctionT "arg515" (TypeVarT "a") (DatatypeT "Bool" []))
          )
        )
      )
    )
  , ( "(Prelude.>)"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg519"
          (DatatypeT "@@hplusTC@@Ord" [TypeVarT "a"])
          (FunctionT "arg517"
                     (TypeVarT "a")
                     (FunctionT "arg518" (TypeVarT "a") (DatatypeT "Bool" []))
          )
        )
      )
    )
  , ( "(Prelude.>=)"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg522"
          (DatatypeT "@@hplusTC@@Ord" [TypeVarT "a"])
          (FunctionT "arg520"
                     (TypeVarT "a")
                     (FunctionT "arg521" (TypeVarT "a") (DatatypeT "Bool" []))
          )
        )
      )
    )
  , ( "@@hplusTCInstance@@0EqBool"
    , Monotype (DatatypeT "@@hplusTC@@Eq" [DatatypeT "Bool" []])
    )
  , ( "@@hplusTCInstance@@0EqChar"
    , Monotype (DatatypeT "@@hplusTC@@Eq" [DatatypeT "Char" []])
    )
  , ( "@@hplusTCInstance@@0EqDouble"
    , Monotype (DatatypeT "@@hplusTC@@Eq" [DatatypeT "Double" []])
    )
  , ( "@@hplusTCInstance@@0EqFloat"
    , Monotype (DatatypeT "@@hplusTC@@Eq" [DatatypeT "Float" []])
    )
  , ( "@@hplusTCInstance@@0EqInt"
    , Monotype (DatatypeT "@@hplusTC@@Eq" [DatatypeT "Int" []])
    )
  , ( "@@hplusTCInstance@@0EqUnit"
    , Monotype (DatatypeT "@@hplusTC@@Eq" [DatatypeT "Unit" []])
    )
  , ( "@@hplusTCInstance@@0NumDouble"
    , Monotype (DatatypeT "@@hplusTC@@Num" [DatatypeT "Double" []])
    )
  , ( "@@hplusTCInstance@@0NumFloat"
    , Monotype (DatatypeT "@@hplusTC@@Num" [DatatypeT "Float" []])
    )
  , ( "@@hplusTCInstance@@0NumInt"
    , Monotype (DatatypeT "@@hplusTC@@Num" [DatatypeT "Int" []])
    )
  , ( "@@hplusTCInstance@@0OrdBool"
    , Monotype (DatatypeT "@@hplusTC@@Ord" [DatatypeT "Bool" []])
    )
  , ( "@@hplusTCInstance@@0OrdChar"
    , Monotype (DatatypeT "@@hplusTC@@Ord" [DatatypeT "Char" []])
    )
  , ( "@@hplusTCInstance@@0OrdDouble"
    , Monotype (DatatypeT "@@hplusTC@@Ord" [DatatypeT "Double" []])
    )
  , ( "@@hplusTCInstance@@0OrdFloat"
    , Monotype (DatatypeT "@@hplusTC@@Ord" [DatatypeT "Float" []])
    )
  , ( "@@hplusTCInstance@@0OrdInt"
    , Monotype (DatatypeT "@@hplusTC@@Ord" [DatatypeT "Int" []])
    )
  , ( "@@hplusTCInstance@@0ShowBool"
    , Monotype (DatatypeT "@@hplusTC@@Show" [DatatypeT "Bool" []])
    )
  , ( "@@hplusTCInstance@@0ShowChar"
    , Monotype (DatatypeT "@@hplusTC@@Show" [DatatypeT "Char" []])
    )
  , ( "@@hplusTCInstance@@0ShowDouble"
    , Monotype (DatatypeT "@@hplusTC@@Show" [DatatypeT "Double" []])
    )
  , ( "@@hplusTCInstance@@0ShowFloat"
    , Monotype (DatatypeT "@@hplusTC@@Show" [DatatypeT "Float" []])
    )
  , ( "@@hplusTCInstance@@0ShowInt"
    , Monotype (DatatypeT "@@hplusTC@@Show" [DatatypeT "Int" []])
    )
  , ( "@@hplusTCInstance@@0ShowUnit"
    , Monotype (DatatypeT "@@hplusTC@@Show" [DatatypeT "Unit" []])
    )
  , ( "@@hplusTCInstance@@289GHC.Show.Show"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "tcarg288"
            (DatatypeT "@@hplusTC@@Show" [TypeVarT "b"])
            (FunctionT
              "tcarg287"
              (DatatypeT "@@hplusTC@@Show" [TypeVarT "a"])
              (DatatypeT "@@hplusTC@@Show"
                         [DatatypeT "Either" [TypeVarT "a", TypeVarT "b"]]
              )
            )
          )
        )
      )
    )
  , ( "@@hplusTCInstance@@292GHC.Read.Read"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "tcarg291"
            (DatatypeT "@@hplusTC@@Read" [TypeVarT "b"])
            (FunctionT
              "tcarg290"
              (DatatypeT "@@hplusTC@@Read" [TypeVarT "a"])
              (DatatypeT "@@hplusTC@@Read"
                         [DatatypeT "Either" [TypeVarT "a", TypeVarT "b"]]
              )
            )
          )
        )
      )
    )
  , ( "@@hplusTCInstance@@295GHC.Classes.Ord"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "tcarg294"
            (DatatypeT "@@hplusTC@@Ord" [TypeVarT "b"])
            (FunctionT
              "tcarg293"
              (DatatypeT "@@hplusTC@@Ord" [TypeVarT "a"])
              (DatatypeT "@@hplusTC@@Ord"
                         [DatatypeT "Either" [TypeVarT "a", TypeVarT "b"]]
              )
            )
          )
        )
      )
    )
  , ( "@@hplusTCInstance@@298GHC.Classes.Eq"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "tcarg297"
            (DatatypeT "@@hplusTC@@Eq" [TypeVarT "b"])
            (FunctionT
              "tcarg296"
              (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
              (DatatypeT "@@hplusTC@@Eq"
                         [DatatypeT "Either" [TypeVarT "a", TypeVarT "b"]]
              )
            )
          )
        )
      )
    )
  , ( "@@hplusTCInstance@@299GHC.Base.Semigroup"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (DatatypeT "@@hplusTC@@Semigroup"
                     [DatatypeT "Either" [TypeVarT "a", TypeVarT "b"]]
          )
        )
      )
    )
  , ( "@@hplusTCInstance@@307Eq"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg306"
          (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
          (DatatypeT "@@hplusTC@@Eq" [DatatypeT "List" [TypeVarT "a"]])
        )
      )
    )
  , ( "@@hplusTCInstance@@68Data.String.IsString"
    , Monotype (DatatypeT "@@hplusTC@@IsString" [DatatypeT "Builder" []])
    )
  , ( "Cons"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "x"
          (TypeVarT "a")
          (FunctionT "xs"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "List" [TypeVarT "a"])
          )
        )
      )
    )
  , ("Data.Bool.False", Monotype (DatatypeT "Bool" []))
  , ("Data.Bool.True" , Monotype (DatatypeT "Bool" []))
  , ( "Data.Bool.bool"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg5"
          (TypeVarT "a")
          (FunctionT "arg6"
                     (TypeVarT "a")
                     (FunctionT "arg7" (DatatypeT "Bool" []) (TypeVarT "a"))
          )
        )
      )
    )
  , ( "Data.Bool.not"
    , Monotype (FunctionT "arg4" (DatatypeT "Bool" []) (DatatypeT "Bool" []))
    )
  , ("Data.Bool.otherwise", Monotype (DatatypeT "Bool" []))
  , ( "Data.ByteString.Builder.byteString"
    , Monotype
      (FunctionT "arg11" (DatatypeT "ByteString" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.byteStringHex"
    , Monotype
      (FunctionT "arg66" (DatatypeT "ByteString" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.char7"
    , Monotype
      (FunctionT "arg32" (DatatypeT "Char" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.char8"
    , Monotype
      (FunctionT "arg34" (DatatypeT "Char" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.charUtf8"
    , Monotype
      (FunctionT "arg36" (DatatypeT "Char" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.doubleBE"
    , Monotype
      (FunctionT "arg23" (DatatypeT "Double" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.doubleDec"
    , Monotype
      (FunctionT "arg50" (DatatypeT "Double" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.doubleHexFixed"
    , Monotype
      (FunctionT "arg65" (DatatypeT "Double" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.doubleLE"
    , Monotype
      (FunctionT "arg31" (DatatypeT "Double" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.floatBE"
    , Monotype
      (FunctionT "arg22" (DatatypeT "Float" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.floatDec"
    , Monotype
      (FunctionT "arg49" (DatatypeT "Float" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.floatHexFixed"
    , Monotype
      (FunctionT "arg64" (DatatypeT "Float" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.floatLE"
    , Monotype
      (FunctionT "arg30" (DatatypeT "Float" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.hPutBuilder"
    , Monotype
      (FunctionT
        "arg9"
        (DatatypeT "Handle" [])
        (FunctionT "arg10"
                   (DatatypeT "Builder" [])
                   (DatatypeT "IO" [DatatypeT "Unit" []])
        )
      )
    )
  , ( "Data.ByteString.Builder.int16BE"
    , Monotype
      (FunctionT "arg16" (DatatypeT "Int16" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int16Dec"
    , Monotype
      (FunctionT "arg39" (DatatypeT "Int16" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int16HexFixed"
    , Monotype
      (FunctionT "arg57" (DatatypeT "Int16" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int16LE"
    , Monotype
      (FunctionT "arg24" (DatatypeT "Int16" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int32BE"
    , Monotype
      (FunctionT "arg17" (DatatypeT "Int32" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int32Dec"
    , Monotype
      (FunctionT "arg40" (DatatypeT "Int32" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int32HexFixed"
    , Monotype
      (FunctionT "arg58" (DatatypeT "Int32" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int32LE"
    , Monotype
      (FunctionT "arg25" (DatatypeT "Int32" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int64BE"
    , Monotype
      (FunctionT "arg18" (DatatypeT "Int64" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int64Dec"
    , Monotype
      (FunctionT "arg41" (DatatypeT "Int64" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int64HexFixed"
    , Monotype
      (FunctionT "arg59" (DatatypeT "Int64" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int64LE"
    , Monotype
      (FunctionT "arg26" (DatatypeT "Int64" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int8"
    , Monotype
      (FunctionT "arg14" (DatatypeT "Int8" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int8Dec"
    , Monotype
      (FunctionT "arg38" (DatatypeT "Int8" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.int8HexFixed"
    , Monotype
      (FunctionT "arg56" (DatatypeT "Int8" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.intDec"
    , Monotype (FunctionT "arg42" (DatatypeT "Int" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.integerDec"
    , Monotype
      (FunctionT "arg43" (DatatypeT "Integer" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.lazyByteString"
    , Monotype
      (FunctionT "arg12" (DatatypeT "ByteString" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.lazyByteStringHex"
    , Monotype
      (FunctionT "arg67" (DatatypeT "ByteString" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.shortByteString"
    , Monotype
      (FunctionT "arg13"
                 (DatatypeT "ShortByteString" [])
                 (DatatypeT "Builder" [])
      )
    )
  , ( "Data.ByteString.Builder.string7"
    , Monotype
      (FunctionT "arg33"
                 (DatatypeT "List" [DatatypeT "Char" []])
                 (DatatypeT "Builder" [])
      )
    )
  , ( "Data.ByteString.Builder.string8"
    , Monotype
      (FunctionT "arg35"
                 (DatatypeT "List" [DatatypeT "Char" []])
                 (DatatypeT "Builder" [])
      )
    )
  , ( "Data.ByteString.Builder.stringUtf8"
    , Monotype
      (FunctionT "arg37"
                 (DatatypeT "List" [DatatypeT "Char" []])
                 (DatatypeT "Builder" [])
      )
    )
  , ( "Data.ByteString.Builder.toLazyByteString"
    , Monotype
      (FunctionT "arg8" (DatatypeT "Builder" []) (DatatypeT "ByteString" []))
    )
  , ( "Data.ByteString.Builder.word16BE"
    , Monotype
      (FunctionT "arg19" (DatatypeT "Word16" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word16Dec"
    , Monotype
      (FunctionT "arg45" (DatatypeT "Word16" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word16Hex"
    , Monotype
      (FunctionT "arg52" (DatatypeT "Word16" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word16HexFixed"
    , Monotype
      (FunctionT "arg61" (DatatypeT "Word16" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word16LE"
    , Monotype
      (FunctionT "arg27" (DatatypeT "Word16" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word32BE"
    , Monotype
      (FunctionT "arg20" (DatatypeT "Word32" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word32Dec"
    , Monotype
      (FunctionT "arg46" (DatatypeT "Word32" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word32Hex"
    , Monotype
      (FunctionT "arg53" (DatatypeT "Word32" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word32HexFixed"
    , Monotype
      (FunctionT "arg62" (DatatypeT "Word32" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word32LE"
    , Monotype
      (FunctionT "arg28" (DatatypeT "Word32" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word64BE"
    , Monotype
      (FunctionT "arg21" (DatatypeT "Word64" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word64Dec"
    , Monotype
      (FunctionT "arg47" (DatatypeT "Word64" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word64Hex"
    , Monotype
      (FunctionT "arg54" (DatatypeT "Word64" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word64HexFixed"
    , Monotype
      (FunctionT "arg63" (DatatypeT "Word64" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word64LE"
    , Monotype
      (FunctionT "arg29" (DatatypeT "Word64" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word8"
    , Monotype
      (FunctionT "arg15" (DatatypeT "Word8" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word8Dec"
    , Monotype
      (FunctionT "arg44" (DatatypeT "Word8" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word8Hex"
    , Monotype
      (FunctionT "arg51" (DatatypeT "Word8" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.word8HexFixed"
    , Monotype
      (FunctionT "arg60" (DatatypeT "Word8" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.wordDec"
    , Monotype
      (FunctionT "arg48" (DatatypeT "Word" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Builder.wordHex"
    , Monotype
      (FunctionT "arg55" (DatatypeT "Word" []) (DatatypeT "Builder" []))
    )
  , ( "Data.ByteString.Lazy.all"
    , Monotype
      (FunctionT
        "arg145"
        (FunctionT "arg147" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT "arg146" (DatatypeT "ByteString" []) (DatatypeT "Bool" []))
      )
    )
  , ( "Data.ByteString.Lazy.any"
    , Monotype
      (FunctionT
        "arg142"
        (FunctionT "arg144" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT "arg143" (DatatypeT "ByteString" []) (DatatypeT "Bool" []))
      )
    )
  , ( "Data.ByteString.Lazy.append"
    , Monotype
      (FunctionT
        "arg92"
        (DatatypeT "ByteString" [])
        (FunctionT "arg93"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.appendFile"
    , Monotype
      (FunctionT
        "arg258"
        (DatatypeT "List" [DatatypeT "Char" []])
        (FunctionT "arg259"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "IO" [DatatypeT "Unit" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.break"
    , Monotype
      (FunctionT
        "arg190"
        (FunctionT "arg192" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT
          "arg191"
          (DatatypeT "ByteString" [])
          (DatatypeT "Pair"
                     [DatatypeT "ByteString" [], DatatypeT "ByteString" []]
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.concat"
    , Monotype
      (FunctionT "arg138"
                 (DatatypeT "List" [DatatypeT "ByteString" []])
                 (DatatypeT "ByteString" [])
      )
    )
  , ( "Data.ByteString.Lazy.concatMap"
    , Monotype
      (FunctionT
        "arg139"
        (FunctionT "arg141" (DatatypeT "Word8" []) (DatatypeT "ByteString" []))
        (FunctionT "arg140"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.cons"
    , Monotype
      (FunctionT
        "arg86"
        (DatatypeT "Word8" [])
        (FunctionT "arg87"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.cons'"
    , Monotype
      (FunctionT
        "arg88"
        (DatatypeT "Word8" [])
        (FunctionT "arg89"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.copy"
    , Monotype
      (FunctionT "arg250"
                 (DatatypeT "ByteString" [])
                 (DatatypeT "ByteString" [])
      )
    )
  , ( "Data.ByteString.Lazy.count"
    , Monotype
      (FunctionT
        "arg240"
        (DatatypeT "Word8" [])
        (FunctionT "arg241" (DatatypeT "ByteString" []) (DatatypeT "Int64" []))
      )
    )
  , ( "Data.ByteString.Lazy.cycle"
    , Monotype
      (FunctionT "arg168"
                 (DatatypeT "ByteString" [])
                 (DatatypeT "ByteString" [])
      )
    )
  , ( "Data.ByteString.Lazy.drop"
    , Monotype
      (FunctionT
        "arg177"
        (DatatypeT "Int64" [])
        (FunctionT "arg178"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.dropWhile"
    , Monotype
      (FunctionT
        "arg184"
        (FunctionT "arg186" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT "arg185"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.elem"
    , Monotype
      (FunctionT
        "arg213"
        (DatatypeT "Word8" [])
        (FunctionT "arg214" (DatatypeT "ByteString" []) (DatatypeT "Bool" []))
      )
    )
  , ( "Data.ByteString.Lazy.elemIndex"
    , Monotype
      (FunctionT
        "arg228"
        (DatatypeT "Word8" [])
        (FunctionT "arg229"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "Maybe" [DatatypeT "Int64" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.elemIndexEnd"
    , Monotype
      (FunctionT
        "arg230"
        (DatatypeT "Word8" [])
        (FunctionT "arg231"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "Maybe" [DatatypeT "Int64" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.elemIndices"
    , Monotype
      (FunctionT
        "arg232"
        (DatatypeT "Word8" [])
        (FunctionT "arg233"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "List" [DatatypeT "Int64" []])
        )
      )
    )
  , ("Data.ByteString.Lazy.empty", Monotype (DatatypeT "ByteString" []))
  , ( "Data.ByteString.Lazy.filter"
    , Monotype
      (FunctionT
        "arg220"
        (FunctionT "arg222" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT "arg221"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.find"
    , Monotype
      (FunctionT
        "arg217"
        (FunctionT "arg219" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT "arg218"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "Maybe" [DatatypeT "Word8" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.findIndex"
    , Monotype
      (FunctionT
        "arg234"
        (FunctionT "arg236" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT "arg235"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "Maybe" [DatatypeT "Int64" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.findIndices"
    , Monotype
      (FunctionT
        "arg237"
        (FunctionT "arg239" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT "arg238"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "List" [DatatypeT "Int64" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.foldl"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg111"
          (FunctionT
            "arg114"
            (TypeVarT "a")
            (FunctionT "arg115" (DatatypeT "Word8" []) (TypeVarT "a"))
          )
          (FunctionT
            "arg112"
            (TypeVarT "a")
            (FunctionT "arg113" (DatatypeT "ByteString" []) (TypeVarT "a"))
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.foldl'"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg116"
          (FunctionT
            "arg119"
            (TypeVarT "a")
            (FunctionT "arg120" (DatatypeT "Word8" []) (TypeVarT "a"))
          )
          (FunctionT
            "arg117"
            (TypeVarT "a")
            (FunctionT "arg118" (DatatypeT "ByteString" []) (TypeVarT "a"))
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.foldl1"
    , Monotype
      (FunctionT
        "arg121"
        (FunctionT
          "arg123"
          (DatatypeT "Word8" [])
          (FunctionT "arg124" (DatatypeT "Word8" []) (DatatypeT "Word8" []))
        )
        (FunctionT "arg122" (DatatypeT "ByteString" []) (DatatypeT "Word8" []))
      )
    )
  , ( "Data.ByteString.Lazy.foldl1'"
    , Monotype
      (FunctionT
        "arg125"
        (FunctionT
          "arg127"
          (DatatypeT "Word8" [])
          (FunctionT "arg128" (DatatypeT "Word8" []) (DatatypeT "Word8" []))
        )
        (FunctionT "arg126" (DatatypeT "ByteString" []) (DatatypeT "Word8" []))
      )
    )
  , ( "Data.ByteString.Lazy.foldlChunks"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg81"
          (FunctionT
            "arg84"
            (TypeVarT "a")
            (FunctionT "arg85" (DatatypeT "ByteString" []) (TypeVarT "a"))
          )
          (FunctionT
            "arg82"
            (TypeVarT "a")
            (FunctionT "arg83" (DatatypeT "ByteString" []) (TypeVarT "a"))
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.foldr"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg129"
          (FunctionT "arg132"
                     (DatatypeT "Word8" [])
                     (FunctionT "arg133" (TypeVarT "a") (TypeVarT "a"))
          )
          (FunctionT
            "arg130"
            (TypeVarT "a")
            (FunctionT "arg131" (DatatypeT "ByteString" []) (TypeVarT "a"))
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.foldr1"
    , Monotype
      (FunctionT
        "arg134"
        (FunctionT
          "arg136"
          (DatatypeT "Word8" [])
          (FunctionT "arg137" (DatatypeT "Word8" []) (DatatypeT "Word8" []))
        )
        (FunctionT "arg135" (DatatypeT "ByteString" []) (DatatypeT "Word8" []))
      )
    )
  , ( "Data.ByteString.Lazy.foldrChunks"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg76"
          (FunctionT "arg79"
                     (DatatypeT "ByteString" [])
                     (FunctionT "arg80" (TypeVarT "a") (TypeVarT "a"))
          )
          (FunctionT
            "arg77"
            (TypeVarT "a")
            (FunctionT "arg78" (DatatypeT "ByteString" []) (TypeVarT "a"))
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.fromChunks"
    , Monotype
      (FunctionT "arg74"
                 (DatatypeT "List" [DatatypeT "ByteString" []])
                 (DatatypeT "ByteString" [])
      )
    )
  , ( "Data.ByteString.Lazy.fromStrict"
    , Monotype
      (FunctionT "arg72" (DatatypeT "ByteString" []) (DatatypeT "ByteString" [])
      )
    )
  , ( "Data.ByteString.Lazy.getContents"
    , Monotype (DatatypeT "IO" [DatatypeT "ByteString" []])
    )
  , ( "Data.ByteString.Lazy.group"
    , Monotype
      (FunctionT "arg193"
                 (DatatypeT "ByteString" [])
                 (DatatypeT "List" [DatatypeT "ByteString" []])
      )
    )
  , ( "Data.ByteString.Lazy.groupBy"
    , Monotype
      (FunctionT
        "arg194"
        (FunctionT
          "arg196"
          (DatatypeT "Word8" [])
          (FunctionT "arg197" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        )
        (FunctionT "arg195"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "List" [DatatypeT "ByteString" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.hGet"
    , Monotype
      (FunctionT
        "arg261"
        (DatatypeT "Handle" [])
        (FunctionT "arg262"
                   (DatatypeT "Int" [])
                   (DatatypeT "IO" [DatatypeT "ByteString" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.hGetContents"
    , Monotype
      (FunctionT "arg260"
                 (DatatypeT "Handle" [])
                 (DatatypeT "IO" [DatatypeT "ByteString" []])
      )
    )
  , ( "Data.ByteString.Lazy.hGetNonBlocking"
    , Monotype
      (FunctionT
        "arg263"
        (DatatypeT "Handle" [])
        (FunctionT "arg264"
                   (DatatypeT "Int" [])
                   (DatatypeT "IO" [DatatypeT "ByteString" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.hPut"
    , Monotype
      (FunctionT
        "arg265"
        (DatatypeT "Handle" [])
        (FunctionT "arg266"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "IO" [DatatypeT "Unit" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.hPutNonBlocking"
    , Monotype
      (FunctionT
        "arg267"
        (DatatypeT "Handle" [])
        (FunctionT "arg268"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "IO" [DatatypeT "ByteString" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.hPutStr"
    , Monotype
      (FunctionT
        "arg269"
        (DatatypeT "Handle" [])
        (FunctionT "arg270"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "IO" [DatatypeT "Unit" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.head"
    , Monotype
      (FunctionT "arg94" (DatatypeT "ByteString" []) (DatatypeT "Word8" []))
    )
  , ( "Data.ByteString.Lazy.index"
    , Monotype
      (FunctionT
        "arg226"
        (DatatypeT "ByteString" [])
        (FunctionT "arg227" (DatatypeT "Int64" []) (DatatypeT "Word8" []))
      )
    )
  , ( "Data.ByteString.Lazy.init"
    , Monotype
      (FunctionT "arg99" (DatatypeT "ByteString" []) (DatatypeT "ByteString" [])
      )
    )
  , ( "Data.ByteString.Lazy.inits"
    , Monotype
      (FunctionT "arg198"
                 (DatatypeT "ByteString" [])
                 (DatatypeT "List" [DatatypeT "ByteString" []])
      )
    )
  , ( "Data.ByteString.Lazy.interact"
    , Monotype
      (FunctionT
        "arg253"
        (FunctionT "arg254"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
        (DatatypeT "IO" [DatatypeT "Unit" []])
      )
    )
  , ( "Data.ByteString.Lazy.intercalate"
    , Monotype
      (FunctionT
        "arg108"
        (DatatypeT "ByteString" [])
        (FunctionT "arg109"
                   (DatatypeT "List" [DatatypeT "ByteString" []])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.intersperse"
    , Monotype
      (FunctionT
        "arg106"
        (DatatypeT "Word8" [])
        (FunctionT "arg107"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.isPrefixOf"
    , Monotype
      (FunctionT
        "arg209"
        (DatatypeT "ByteString" [])
        (FunctionT "arg210" (DatatypeT "ByteString" []) (DatatypeT "Bool" []))
      )
    )
  , ( "Data.ByteString.Lazy.isSuffixOf"
    , Monotype
      (FunctionT
        "arg211"
        (DatatypeT "ByteString" [])
        (FunctionT "arg212" (DatatypeT "ByteString" []) (DatatypeT "Bool" []))
      )
    )
  , ( "Data.ByteString.Lazy.iterate"
    , Monotype
      (FunctionT
        "arg169"
        (FunctionT "arg171" (DatatypeT "Word8" []) (DatatypeT "Word8" []))
        (FunctionT "arg170" (DatatypeT "Word8" []) (DatatypeT "ByteString" []))
      )
    )
  , ( "Data.ByteString.Lazy.last"
    , Monotype
      (FunctionT "arg97" (DatatypeT "ByteString" []) (DatatypeT "Word8" []))
    )
  , ( "Data.ByteString.Lazy.length"
    , Monotype
      (FunctionT "arg101" (DatatypeT "ByteString" []) (DatatypeT "Int64" []))
    )
  , ( "Data.ByteString.Lazy.map"
    , Monotype
      (FunctionT
        "arg102"
        (FunctionT "arg104" (DatatypeT "Word8" []) (DatatypeT "Word8" []))
        (FunctionT "arg103"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.mapAccumL"
    , ForallT
      "acc"
      (Monotype
        (FunctionT
          "arg155"
          (FunctionT
            "arg158"
            (TypeVarT "acc")
            (FunctionT
              "arg159"
              (DatatypeT "Word8" [])
              (DatatypeT "Pair" [TypeVarT "acc", DatatypeT "Word8" []])
            )
          )
          (FunctionT
            "arg156"
            (TypeVarT "acc")
            (FunctionT
              "arg157"
              (DatatypeT "ByteString" [])
              (DatatypeT "Pair" [TypeVarT "acc", DatatypeT "ByteString" []])
            )
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.mapAccumR"
    , ForallT
      "acc"
      (Monotype
        (FunctionT
          "arg160"
          (FunctionT
            "arg163"
            (TypeVarT "acc")
            (FunctionT
              "arg164"
              (DatatypeT "Word8" [])
              (DatatypeT "Pair" [TypeVarT "acc", DatatypeT "Word8" []])
            )
          )
          (FunctionT
            "arg161"
            (TypeVarT "acc")
            (FunctionT
              "arg162"
              (DatatypeT "ByteString" [])
              (DatatypeT "Pair" [TypeVarT "acc", DatatypeT "ByteString" []])
            )
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.maximum"
    , Monotype
      (FunctionT "arg148" (DatatypeT "ByteString" []) (DatatypeT "Word8" []))
    )
  , ( "Data.ByteString.Lazy.minimum"
    , Monotype
      (FunctionT "arg149" (DatatypeT "ByteString" []) (DatatypeT "Word8" []))
    )
  , ( "Data.ByteString.Lazy.notElem"
    , Monotype
      (FunctionT
        "arg215"
        (DatatypeT "Word8" [])
        (FunctionT "arg216" (DatatypeT "ByteString" []) (DatatypeT "Bool" []))
      )
    )
  , ( "Data.ByteString.Lazy.null"
    , Monotype
      (FunctionT "arg100" (DatatypeT "ByteString" []) (DatatypeT "Bool" []))
    )
  , ( "Data.ByteString.Lazy.pack"
    , Monotype
      (FunctionT "arg70"
                 (DatatypeT "List" [DatatypeT "Word8" []])
                 (DatatypeT "ByteString" [])
      )
    )
  , ( "Data.ByteString.Lazy.partition"
    , Monotype
      (FunctionT
        "arg223"
        (FunctionT "arg225" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT
          "arg224"
          (DatatypeT "ByteString" [])
          (DatatypeT "Pair"
                     [DatatypeT "ByteString" [], DatatypeT "ByteString" []]
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.putStr"
    , Monotype
      (FunctionT "arg251"
                 (DatatypeT "ByteString" [])
                 (DatatypeT "IO" [DatatypeT "Unit" []])
      )
    )
  , ( "Data.ByteString.Lazy.putStrLn"
    , Monotype
      (FunctionT "arg252"
                 (DatatypeT "ByteString" [])
                 (DatatypeT "IO" [DatatypeT "Unit" []])
      )
    )
  , ( "Data.ByteString.Lazy.readFile"
    , Monotype
      (FunctionT "arg255"
                 (DatatypeT "List" [DatatypeT "Char" []])
                 (DatatypeT "IO" [DatatypeT "ByteString" []])
      )
    )
  , ( "Data.ByteString.Lazy.repeat"
    , Monotype
      (FunctionT "arg165" (DatatypeT "Word8" []) (DatatypeT "ByteString" []))
    )
  , ( "Data.ByteString.Lazy.replicate"
    , Monotype
      (FunctionT
        "arg166"
        (DatatypeT "Int64" [])
        (FunctionT "arg167" (DatatypeT "Word8" []) (DatatypeT "ByteString" []))
      )
    )
  , ( "Data.ByteString.Lazy.reverse"
    , Monotype
      (FunctionT "arg105"
                 (DatatypeT "ByteString" [])
                 (DatatypeT "ByteString" [])
      )
    )
  , ( "Data.ByteString.Lazy.scanl"
    , Monotype
      (FunctionT
        "arg150"
        (FunctionT
          "arg153"
          (DatatypeT "Word8" [])
          (FunctionT "arg154" (DatatypeT "Word8" []) (DatatypeT "Word8" []))
        )
        (FunctionT
          "arg151"
          (DatatypeT "Word8" [])
          (FunctionT "arg152"
                     (DatatypeT "ByteString" [])
                     (DatatypeT "ByteString" [])
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.singleton"
    , Monotype
      (FunctionT "arg69" (DatatypeT "Word8" []) (DatatypeT "ByteString" []))
    )
  , ( "Data.ByteString.Lazy.snoc"
    , Monotype
      (FunctionT
        "arg90"
        (DatatypeT "ByteString" [])
        (FunctionT "arg91" (DatatypeT "Word8" []) (DatatypeT "ByteString" []))
      )
    )
  , ( "Data.ByteString.Lazy.span"
    , Monotype
      (FunctionT
        "arg187"
        (FunctionT "arg189" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT
          "arg188"
          (DatatypeT "ByteString" [])
          (DatatypeT "Pair"
                     [DatatypeT "ByteString" [], DatatypeT "ByteString" []]
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.split"
    , Monotype
      (FunctionT
        "arg204"
        (DatatypeT "Word8" [])
        (FunctionT "arg205"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "List" [DatatypeT "ByteString" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.splitAt"
    , Monotype
      (FunctionT
        "arg179"
        (DatatypeT "Int64" [])
        (FunctionT
          "arg180"
          (DatatypeT "ByteString" [])
          (DatatypeT "Pair"
                     [DatatypeT "ByteString" [], DatatypeT "ByteString" []]
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.splitWith"
    , Monotype
      (FunctionT
        "arg206"
        (FunctionT "arg208" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT "arg207"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "List" [DatatypeT "ByteString" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.stripPrefix"
    , Monotype
      (FunctionT
        "arg200"
        (DatatypeT "ByteString" [])
        (FunctionT "arg201"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "Maybe" [DatatypeT "ByteString" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.stripSuffix"
    , Monotype
      (FunctionT
        "arg202"
        (DatatypeT "ByteString" [])
        (FunctionT "arg203"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "Maybe" [DatatypeT "ByteString" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.tail"
    , Monotype
      (FunctionT "arg98" (DatatypeT "ByteString" []) (DatatypeT "ByteString" [])
      )
    )
  , ( "Data.ByteString.Lazy.tails"
    , Monotype
      (FunctionT "arg199"
                 (DatatypeT "ByteString" [])
                 (DatatypeT "List" [DatatypeT "ByteString" []])
      )
    )
  , ( "Data.ByteString.Lazy.take"
    , Monotype
      (FunctionT
        "arg175"
        (DatatypeT "Int64" [])
        (FunctionT "arg176"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.takeWhile"
    , Monotype
      (FunctionT
        "arg181"
        (FunctionT "arg183" (DatatypeT "Word8" []) (DatatypeT "Bool" []))
        (FunctionT "arg182"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "ByteString" [])
        )
      )
    )
  , ( "Data.ByteString.Lazy.toChunks"
    , Monotype
      (FunctionT "arg75"
                 (DatatypeT "ByteString" [])
                 (DatatypeT "List" [DatatypeT "ByteString" []])
      )
    )
  , ( "Data.ByteString.Lazy.toStrict"
    , Monotype
      (FunctionT "arg73" (DatatypeT "ByteString" []) (DatatypeT "ByteString" [])
      )
    )
  , ( "Data.ByteString.Lazy.transpose"
    , Monotype
      (FunctionT "arg110"
                 (DatatypeT "List" [DatatypeT "ByteString" []])
                 (DatatypeT "List" [DatatypeT "ByteString" []])
      )
    )
  , ( "Data.ByteString.Lazy.uncons"
    , Monotype
      (FunctionT
        "arg95"
        (DatatypeT "ByteString" [])
        (DatatypeT
          "Maybe"
          [DatatypeT "Pair" [DatatypeT "Word8" [], DatatypeT "ByteString" []]]
        )
      )
    )
  , ( "Data.ByteString.Lazy.unfoldr"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg172"
          (FunctionT
            "arg174"
            (TypeVarT "a")
            (DatatypeT "Maybe"
                       [DatatypeT "Pair" [DatatypeT "Word8" [], TypeVarT "a"]]
            )
          )
          (FunctionT "arg173" (TypeVarT "a") (DatatypeT "ByteString" []))
        )
      )
    )
  , ( "Data.ByteString.Lazy.unpack"
    , Monotype
      (FunctionT "arg71"
                 (DatatypeT "ByteString" [])
                 (DatatypeT "List" [DatatypeT "Word8" []])
      )
    )
  , ( "Data.ByteString.Lazy.unsnoc"
    , Monotype
      (FunctionT
        "arg96"
        (DatatypeT "ByteString" [])
        (DatatypeT
          "Maybe"
          [DatatypeT "Pair" [DatatypeT "ByteString" [], DatatypeT "Word8" []]]
        )
      )
    )
  , ( "Data.ByteString.Lazy.unzip"
    , Monotype
      (FunctionT
        "arg249"
        (DatatypeT
          "List"
          [DatatypeT "Pair" [DatatypeT "Word8" [], DatatypeT "Word8" []]]
        )
        (DatatypeT "Pair" [DatatypeT "ByteString" [], DatatypeT "ByteString" []]
        )
      )
    )
  , ( "Data.ByteString.Lazy.writeFile"
    , Monotype
      (FunctionT
        "arg256"
        (DatatypeT "List" [DatatypeT "Char" []])
        (FunctionT "arg257"
                   (DatatypeT "ByteString" [])
                   (DatatypeT "IO" [DatatypeT "Unit" []])
        )
      )
    )
  , ( "Data.ByteString.Lazy.zip"
    , Monotype
      (FunctionT
        "arg242"
        (DatatypeT "ByteString" [])
        (FunctionT
          "arg243"
          (DatatypeT "ByteString" [])
          (DatatypeT
            "List"
            [DatatypeT "Pair" [DatatypeT "Word8" [], DatatypeT "Word8" []]]
          )
        )
      )
    )
  , ( "Data.ByteString.Lazy.zipWith"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg244"
          (FunctionT
            "arg247"
            (DatatypeT "Word8" [])
            (FunctionT "arg248" (DatatypeT "Word8" []) (TypeVarT "a"))
          )
          (FunctionT
            "arg245"
            (DatatypeT "ByteString" [])
            (FunctionT "arg246"
                       (DatatypeT "ByteString" [])
                       (DatatypeT "List" [TypeVarT "a"])
            )
          )
        )
      )
    )
  , ( "Data.Either.Left"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "arg271"
                     (TypeVarT "a")
                     (DatatypeT "Either" [TypeVarT "a", TypeVarT "b"])
          )
        )
      )
    )
  , ( "Data.Either.Right"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "arg272"
                     (TypeVarT "b")
                     (DatatypeT "Either" [TypeVarT "a", TypeVarT "b"])
          )
        )
      )
    )
  , ( "Data.Either.either"
    , ForallT
      "a"
      (ForallT
        "b"
        (ForallT
          "c"
          (Monotype
            (FunctionT
              "arg273"
              (FunctionT "arg277" (TypeVarT "a") (TypeVarT "c"))
              (FunctionT
                "arg274"
                (FunctionT "arg276" (TypeVarT "b") (TypeVarT "c"))
                (FunctionT "arg275"
                           (DatatypeT "Either" [TypeVarT "a", TypeVarT "b"])
                           (TypeVarT "c")
                )
              )
            )
          )
        )
      )
    )
  , ( "Data.Either.fromLeft"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg282"
            (TypeVarT "a")
            (FunctionT "arg283"
                       (DatatypeT "Either" [TypeVarT "a", TypeVarT "b"])
                       (TypeVarT "a")
            )
          )
        )
      )
    )
  , ( "Data.Either.fromRight"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg284"
            (TypeVarT "b")
            (FunctionT "arg285"
                       (DatatypeT "Either" [TypeVarT "a", TypeVarT "b"])
                       (TypeVarT "b")
            )
          )
        )
      )
    )
  , ( "Data.Either.isLeft"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "arg280"
                     (DatatypeT "Either" [TypeVarT "a", TypeVarT "b"])
                     (DatatypeT "Bool" [])
          )
        )
      )
    )
  , ( "Data.Either.isRight"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "arg281"
                     (DatatypeT "Either" [TypeVarT "a", TypeVarT "b"])
                     (DatatypeT "Bool" [])
          )
        )
      )
    )
  , ( "Data.Either.lefts"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg278"
            (DatatypeT "List" [DatatypeT "Either" [TypeVarT "a", TypeVarT "b"]])
            (DatatypeT "List" [TypeVarT "a"])
          )
        )
      )
    )
  , ( "Data.Either.partitionEithers"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg286"
            (DatatypeT "List" [DatatypeT "Either" [TypeVarT "a", TypeVarT "b"]])
            (DatatypeT
              "Pair"
              [DatatypeT "List" [TypeVarT "a"], DatatypeT "List" [TypeVarT "b"]]
            )
          )
        )
      )
    )
  , ( "Data.Either.rights"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg279"
            (DatatypeT "List" [DatatypeT "Either" [TypeVarT "a", TypeVarT "b"]])
            (DatatypeT "List" [TypeVarT "b"])
          )
        )
      )
    )
  , ( "Data.Function.flip"
    , ForallT
      "a"
      (ForallT
        "b"
        (ForallT
          "c"
          (Monotype
            (FunctionT
              "arg314"
              (FunctionT "arg317"
                         (TypeVarT "a")
                         (FunctionT "arg318" (TypeVarT "b") (TypeVarT "c"))
              )
              (FunctionT "arg315"
                         (TypeVarT "b")
                         (FunctionT "arg316" (TypeVarT "a") (TypeVarT "c"))
              )
            )
          )
        )
      )
    )
  , ( "Data.Function.id"
    , ForallT "a" (Monotype (FunctionT "arg308" (TypeVarT "a") (TypeVarT "a")))
    )
  , ( "Data.Function.on"
    , ForallT
      "a"
      (ForallT
        "b"
        (ForallT
          "c"
          (Monotype
            (FunctionT
              "arg322"
              (FunctionT "arg327"
                         (TypeVarT "b")
                         (FunctionT "arg328" (TypeVarT "b") (TypeVarT "c"))
              )
              (FunctionT
                "arg323"
                (FunctionT "arg326" (TypeVarT "a") (TypeVarT "b"))
                (FunctionT "arg324"
                           (TypeVarT "a")
                           (FunctionT "arg325" (TypeVarT "a") (TypeVarT "c"))
                )
              )
            )
          )
        )
      )
    )
  , ( "Data.List.group"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg330"
          (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
          (FunctionT "arg329"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "List" [DatatypeT "List" [TypeVarT "a"]])
          )
        )
      )
    )
  , ( "Data.Maybe.Just"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg331" (TypeVarT "a") (DatatypeT "Maybe" [TypeVarT "a"]))
      )
    )
  , ( "Data.Maybe.Nothing"
    , ForallT "a" (Monotype (DatatypeT "Maybe" [TypeVarT "a"]))
    )
  , ( "Data.Maybe.catMaybes"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg343"
                   (DatatypeT "List" [DatatypeT "Maybe" [TypeVarT "a"]])
                   (DatatypeT "List" [TypeVarT "a"])
        )
      )
    )
  , ( "Data.Maybe.fromJust"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg338" (DatatypeT "Maybe" [TypeVarT "a"]) (TypeVarT "a"))
      )
    )
  , ( "Data.Maybe.fromMaybe"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg339"
          (TypeVarT "a")
          (FunctionT "arg340" (DatatypeT "Maybe" [TypeVarT "a"]) (TypeVarT "a"))
        )
      )
    )
  , ( "Data.Maybe.isJust"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg336"
                   (DatatypeT "Maybe" [TypeVarT "a"])
                   (DatatypeT "Bool" [])
        )
      )
    )
  , ( "Data.Maybe.isNothing"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg337"
                   (DatatypeT "Maybe" [TypeVarT "a"])
                   (DatatypeT "Bool" [])
        )
      )
    )
  , ( "Data.Maybe.listToMaybe"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg341"
                   (DatatypeT "List" [TypeVarT "a"])
                   (DatatypeT "Maybe" [TypeVarT "a"])
        )
      )
    )
  , ( "Data.Maybe.mapMaybe"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg344"
            (FunctionT "arg346"
                       (TypeVarT "a")
                       (DatatypeT "Maybe" [TypeVarT "b"])
            )
            (FunctionT "arg345"
                       (DatatypeT "List" [TypeVarT "a"])
                       (DatatypeT "List" [TypeVarT "b"])
            )
          )
        )
      )
    )
  , ( "Data.Maybe.maybe"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg332"
            (TypeVarT "b")
            (FunctionT
              "arg333"
              (FunctionT "arg335" (TypeVarT "a") (TypeVarT "b"))
              (FunctionT "arg334"
                         (DatatypeT "Maybe" [TypeVarT "a"])
                         (TypeVarT "b")
              )
            )
          )
        )
      )
    )
  , ( "Data.Maybe.maybeToList"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg342"
                   (DatatypeT "Maybe" [TypeVarT "a"])
                   (DatatypeT "List" [TypeVarT "a"])
        )
      )
    )
  , ( "Data.String.fromString"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg348"
          (DatatypeT "@@hplusTC@@IsString" [TypeVarT "a"])
          (FunctionT "arg347"
                     (DatatypeT "List" [DatatypeT "Char" []])
                     (TypeVarT "a")
          )
        )
      )
    )
  , ( "Data.String.lines"
    , Monotype
      (FunctionT "arg349"
                 (DatatypeT "List" [DatatypeT "Char" []])
                 (DatatypeT "List" [DatatypeT "List" [DatatypeT "Char" []]])
      )
    )
  , ( "Data.String.unlines"
    , Monotype
      (FunctionT "arg351"
                 (DatatypeT "List" [DatatypeT "List" [DatatypeT "Char" []]])
                 (DatatypeT "List" [DatatypeT "Char" []])
      )
    )
  , ( "Data.String.unwords"
    , Monotype
      (FunctionT "arg352"
                 (DatatypeT "List" [DatatypeT "List" [DatatypeT "Char" []]])
                 (DatatypeT "List" [DatatypeT "Char" []])
      )
    )
  , ( "Data.String.words"
    , Monotype
      (FunctionT "arg350"
                 (DatatypeT "List" [DatatypeT "Char" []])
                 (DatatypeT "List" [DatatypeT "List" [DatatypeT "Char" []]])
      )
    )
  , ( "Data.Tuple.curry"
    , ForallT
      "a"
      (ForallT
        "b"
        (ForallT
          "c"
          (Monotype
            (FunctionT
              "arg355"
              (FunctionT "arg358"
                         (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
                         (TypeVarT "c")
              )
              (FunctionT "arg356"
                         (TypeVarT "a")
                         (FunctionT "arg357" (TypeVarT "b") (TypeVarT "c"))
              )
            )
          )
        )
      )
    )
  , ( "Data.Tuple.fst"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "arg353"
                     (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
                     (TypeVarT "a")
          )
        )
      )
    )
  , ( "Data.Tuple.snd"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "arg354"
                     (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
                     (TypeVarT "b")
          )
        )
      )
    )
  , ( "Data.Tuple.swap"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "arg363"
                     (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
                     (DatatypeT "Pair" [TypeVarT "b", TypeVarT "a"])
          )
        )
      )
    )
  , ( "Data.Tuple.uncurry"
    , ForallT
      "a"
      (ForallT
        "b"
        (ForallT
          "c"
          (Monotype
            (FunctionT
              "arg359"
              (FunctionT "arg361"
                         (TypeVarT "a")
                         (FunctionT "arg362" (TypeVarT "b") (TypeVarT "c"))
              )
              (FunctionT "arg360"
                         (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
                         (TypeVarT "c")
              )
            )
          )
        )
      )
    )
  , ( "GHC.Char.chr"
    , Monotype (FunctionT "arg364" (DatatypeT "Int" []) (DatatypeT "Char" []))
    )
  , ( "GHC.Char.eqChar"
    , Monotype
      (FunctionT
        "arg365"
        (DatatypeT "Char" [])
        (FunctionT "arg366" (DatatypeT "Char" []) (DatatypeT "Bool" []))
      )
    )
  , ( "GHC.Char.neChar"
    , Monotype
      (FunctionT
        "arg367"
        (DatatypeT "Char" [])
        (FunctionT "arg368" (DatatypeT "Char" []) (DatatypeT "Bool" []))
      )
    )
  , ( "GHC.List.all"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg474"
          (FunctionT "arg476" (TypeVarT "a") (DatatypeT "Bool" []))
          (FunctionT "arg475"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "Bool" [])
          )
        )
      )
    )
  , ( "GHC.List.and"
    , Monotype
      (FunctionT "arg469"
                 (DatatypeT "List" [DatatypeT "Bool" []])
                 (DatatypeT "Bool" [])
      )
    )
  , ( "GHC.List.any"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg471"
          (FunctionT "arg473" (TypeVarT "a") (DatatypeT "Bool" []))
          (FunctionT "arg472"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "Bool" [])
          )
        )
      )
    )
  , ( "GHC.List.break"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg465"
          (FunctionT "arg467" (TypeVarT "a") (DatatypeT "Bool" []))
          (FunctionT
            "arg466"
            (DatatypeT "List" [TypeVarT "a"])
            (DatatypeT
              "Pair"
              [DatatypeT "List" [TypeVarT "a"], DatatypeT "List" [TypeVarT "a"]]
            )
          )
        )
      )
    )
  , ( "GHC.List.concat"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg377"
                   (DatatypeT "List" [DatatypeT "List" [TypeVarT "a"]])
                   (DatatypeT "List" [TypeVarT "a"])
        )
      )
    )
  , ( "GHC.List.concatMap"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg486"
            (FunctionT "arg488" (TypeVarT "a") (DatatypeT "List" [TypeVarT "b"])
            )
            (FunctionT "arg487"
                       (DatatypeT "List" [TypeVarT "a"])
                       (DatatypeT "List" [TypeVarT "b"])
            )
          )
        )
      )
    )
  , ( "GHC.List.cycle"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg441"
                   (DatatypeT "List" [TypeVarT "a"])
                   (DatatypeT "List" [TypeVarT "a"])
        )
      )
    )
  , ( "GHC.List.drop"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg444"
          (DatatypeT "Int" [])
          (FunctionT "arg445"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "List" [TypeVarT "a"])
          )
        )
      )
    )
  , ( "GHC.List.dropWhile"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg459"
          (FunctionT "arg461" (TypeVarT "a") (DatatypeT "Bool" []))
          (FunctionT "arg460"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "List" [TypeVarT "a"])
          )
        )
      )
    )
  , ( "GHC.List.elem"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg479"
          (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
          (FunctionT
            "arg477"
            (TypeVarT "a")
            (FunctionT "arg478"
                       (DatatypeT "List" [TypeVarT "a"])
                       (DatatypeT "Bool" [])
            )
          )
        )
      )
    )
  , ( "GHC.List.filter"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg374"
          (FunctionT "arg376" (TypeVarT "a") (DatatypeT "Bool" []))
          (FunctionT "arg375"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "List" [TypeVarT "a"])
          )
        )
      )
    )
  , ( "GHC.List.foldl'"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg387"
            (FunctionT "arg390"
                       (TypeVarT "b")
                       (FunctionT "arg391" (TypeVarT "a") (TypeVarT "b"))
            )
            (FunctionT
              "arg388"
              (TypeVarT "b")
              (FunctionT "arg389"
                         (DatatypeT "List" [TypeVarT "a"])
                         (TypeVarT "b")
              )
            )
          )
        )
      )
    )
  , ( "GHC.List.foldl1"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg392"
          (FunctionT "arg394"
                     (TypeVarT "a")
                     (FunctionT "arg395" (TypeVarT "a") (TypeVarT "a"))
          )
          (FunctionT "arg393" (DatatypeT "List" [TypeVarT "a"]) (TypeVarT "a"))
        )
      )
    )
  , ( "GHC.List.foldl1'"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg396"
          (FunctionT "arg398"
                     (TypeVarT "a")
                     (FunctionT "arg399" (TypeVarT "a") (TypeVarT "a"))
          )
          (FunctionT "arg397" (DatatypeT "List" [TypeVarT "a"]) (TypeVarT "a"))
        )
      )
    )
  , ( "GHC.List.foldr"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg414"
            (FunctionT "arg417"
                       (TypeVarT "a")
                       (FunctionT "arg418" (TypeVarT "b") (TypeVarT "b"))
            )
            (FunctionT
              "arg415"
              (TypeVarT "b")
              (FunctionT "arg416"
                         (DatatypeT "List" [TypeVarT "a"])
                         (TypeVarT "b")
              )
            )
          )
        )
      )
    )
  , ( "GHC.List.foldr1"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg419"
          (FunctionT "arg421"
                     (TypeVarT "a")
                     (FunctionT "arg422" (TypeVarT "a") (TypeVarT "a"))
          )
          (FunctionT "arg420" (DatatypeT "List" [TypeVarT "a"]) (TypeVarT "a"))
        )
      )
    )
  , ( "GHC.List.head"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg378" (DatatypeT "List" [TypeVarT "a"]) (TypeVarT "a"))
      )
    )
  , ( "GHC.List.init"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg381"
                   (DatatypeT "List" [TypeVarT "a"])
                   (DatatypeT "List" [TypeVarT "a"])
        )
      )
    )
  , ( "GHC.List.iterate"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg432"
          (FunctionT "arg434" (TypeVarT "a") (TypeVarT "a"))
          (FunctionT "arg433" (TypeVarT "a") (DatatypeT "List" [TypeVarT "a"]))
        )
      )
    )
  , ( "GHC.List.iterate'"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg435"
          (FunctionT "arg437" (TypeVarT "a") (TypeVarT "a"))
          (FunctionT "arg436" (TypeVarT "a") (DatatypeT "List" [TypeVarT "a"]))
        )
      )
    )
  , ( "GHC.List.last"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg379" (DatatypeT "List" [TypeVarT "a"]) (TypeVarT "a"))
      )
    )
  , ( "GHC.List.length"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg384"
                   (DatatypeT "List" [TypeVarT "a"])
                   (DatatypeT "Int" [])
        )
      )
    )
  , ( "GHC.List.lookup"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "tcarg485"
            (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
            (FunctionT
              "arg483"
              (TypeVarT "a")
              (FunctionT
                "arg484"
                (DatatypeT "List"
                           [DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"]]
                )
                (DatatypeT "Maybe" [TypeVarT "b"])
              )
            )
          )
        )
      )
    )
  , ( "GHC.List.map"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg369"
            (FunctionT "arg371" (TypeVarT "a") (TypeVarT "b"))
            (FunctionT "arg370"
                       (DatatypeT "List" [TypeVarT "a"])
                       (DatatypeT "List" [TypeVarT "b"])
            )
          )
        )
      )
    )
  , ( "GHC.List.maximum"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg451"
          (DatatypeT "@@hplusTC@@Ord" [TypeVarT "a"])
          (FunctionT "arg450" (DatatypeT "List" [TypeVarT "a"]) (TypeVarT "a"))
        )
      )
    )
  , ( "GHC.List.minimum"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg453"
          (DatatypeT "@@hplusTC@@Ord" [TypeVarT "a"])
          (FunctionT "arg452" (DatatypeT "List" [TypeVarT "a"]) (TypeVarT "a"))
        )
      )
    )
  , ( "GHC.List.notElem"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg482"
          (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
          (FunctionT
            "arg480"
            (TypeVarT "a")
            (FunctionT "arg481"
                       (DatatypeT "List" [TypeVarT "a"])
                       (DatatypeT "Bool" [])
            )
          )
        )
      )
    )
  , ( "GHC.List.null"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg383"
                   (DatatypeT "List" [TypeVarT "a"])
                   (DatatypeT "Bool" [])
        )
      )
    )
  , ( "GHC.List.or"
    , Monotype
      (FunctionT "arg470"
                 (DatatypeT "List" [DatatypeT "Bool" []])
                 (DatatypeT "Bool" [])
      )
    )
  , ( "GHC.List.product"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg449"
          (DatatypeT "@@hplusTC@@Num" [TypeVarT "a"])
          (FunctionT "arg448" (DatatypeT "List" [TypeVarT "a"]) (TypeVarT "a"))
        )
      )
    )
  , ( "GHC.List.repeat"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg438" (TypeVarT "a") (DatatypeT "List" [TypeVarT "a"]))
      )
    )
  , ( "GHC.List.replicate"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg439"
          (DatatypeT "Int" [])
          (FunctionT "arg440" (TypeVarT "a") (DatatypeT "List" [TypeVarT "a"]))
        )
      )
    )
  , ( "GHC.List.reverse"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg468"
                   (DatatypeT "List" [TypeVarT "a"])
                   (DatatypeT "List" [TypeVarT "a"])
        )
      )
    )
  , ( "GHC.List.scanl"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg400"
            (FunctionT "arg403"
                       (TypeVarT "b")
                       (FunctionT "arg404" (TypeVarT "a") (TypeVarT "b"))
            )
            (FunctionT
              "arg401"
              (TypeVarT "b")
              (FunctionT "arg402"
                         (DatatypeT "List" [TypeVarT "a"])
                         (DatatypeT "List" [TypeVarT "b"])
              )
            )
          )
        )
      )
    )
  , ( "GHC.List.scanl'"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg409"
            (FunctionT "arg412"
                       (TypeVarT "b")
                       (FunctionT "arg413" (TypeVarT "a") (TypeVarT "b"))
            )
            (FunctionT
              "arg410"
              (TypeVarT "b")
              (FunctionT "arg411"
                         (DatatypeT "List" [TypeVarT "a"])
                         (DatatypeT "List" [TypeVarT "b"])
              )
            )
          )
        )
      )
    )
  , ( "GHC.List.scanl1"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg405"
          (FunctionT "arg407"
                     (TypeVarT "a")
                     (FunctionT "arg408" (TypeVarT "a") (TypeVarT "a"))
          )
          (FunctionT "arg406"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "List" [TypeVarT "a"])
          )
        )
      )
    )
  , ( "GHC.List.scanr"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg423"
            (FunctionT "arg426"
                       (TypeVarT "a")
                       (FunctionT "arg427" (TypeVarT "b") (TypeVarT "b"))
            )
            (FunctionT
              "arg424"
              (TypeVarT "b")
              (FunctionT "arg425"
                         (DatatypeT "List" [TypeVarT "a"])
                         (DatatypeT "List" [TypeVarT "b"])
              )
            )
          )
        )
      )
    )
  , ( "GHC.List.scanr1"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg428"
          (FunctionT "arg430"
                     (TypeVarT "a")
                     (FunctionT "arg431" (TypeVarT "a") (TypeVarT "a"))
          )
          (FunctionT "arg429"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "List" [TypeVarT "a"])
          )
        )
      )
    )
  , ( "GHC.List.span"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg462"
          (FunctionT "arg464" (TypeVarT "a") (DatatypeT "Bool" []))
          (FunctionT
            "arg463"
            (DatatypeT "List" [TypeVarT "a"])
            (DatatypeT
              "Pair"
              [DatatypeT "List" [TypeVarT "a"], DatatypeT "List" [TypeVarT "a"]]
            )
          )
        )
      )
    )
  , ( "GHC.List.splitAt"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg454"
          (DatatypeT "Int" [])
          (FunctionT
            "arg455"
            (DatatypeT "List" [TypeVarT "a"])
            (DatatypeT
              "Pair"
              [DatatypeT "List" [TypeVarT "a"], DatatypeT "List" [TypeVarT "a"]]
            )
          )
        )
      )
    )
  , ( "GHC.List.sum"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg447"
          (DatatypeT "@@hplusTC@@Num" [TypeVarT "a"])
          (FunctionT "arg446" (DatatypeT "List" [TypeVarT "a"]) (TypeVarT "a"))
        )
      )
    )
  , ( "GHC.List.tail"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg380"
                   (DatatypeT "List" [TypeVarT "a"])
                   (DatatypeT "List" [TypeVarT "a"])
        )
      )
    )
  , ( "GHC.List.take"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg442"
          (DatatypeT "Int" [])
          (FunctionT "arg443"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "List" [TypeVarT "a"])
          )
        )
      )
    )
  , ( "GHC.List.takeWhile"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg456"
          (FunctionT "arg458" (TypeVarT "a") (DatatypeT "Bool" []))
          (FunctionT "arg457"
                     (DatatypeT "List" [TypeVarT "a"])
                     (DatatypeT "List" [TypeVarT "a"])
          )
        )
      )
    )
  , ( "GHC.List.uncons"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg382"
          (DatatypeT "List" [TypeVarT "a"])
          (DatatypeT
            "Maybe"
            [DatatypeT "Pair" [TypeVarT "a", DatatypeT "List" [TypeVarT "a"]]]
          )
        )
      )
    )
  , ( "GHC.List.unzip"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg506"
            (DatatypeT "List" [DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"]])
            (DatatypeT
              "Pair"
              [DatatypeT "List" [TypeVarT "a"], DatatypeT "List" [TypeVarT "b"]]
            )
          )
        )
      )
    )
  , ( "GHC.List.unzip3"
    , ForallT
      "a"
      (ForallT
        "b"
        (ForallT
          "c"
          (Monotype
            (FunctionT
              "arg507"
              (DatatypeT
                "List"
                [ DatatypeT
                    "Pair"
                    [ DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"]
                    , TypeVarT "c"
                    ]
                ]
              )
              (DatatypeT
                "Pair"
                [ DatatypeT
                  "Pair"
                  [ DatatypeT "List" [TypeVarT "a"]
                  , DatatypeT "List" [TypeVarT "b"]
                  ]
                , DatatypeT "List" [TypeVarT "c"]
                ]
              )
            )
          )
        )
      )
    )
  , ( "GHC.List.zip"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg489"
            (DatatypeT "List" [TypeVarT "a"])
            (FunctionT
              "arg490"
              (DatatypeT "List" [TypeVarT "b"])
              (DatatypeT "List" [DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"]])
            )
          )
        )
      )
    )
  , ( "GHC.List.zip3"
    , ForallT
      "a"
      (ForallT
        "b"
        (ForallT
          "c"
          (Monotype
            (FunctionT
              "arg491"
              (DatatypeT "List" [TypeVarT "a"])
              (FunctionT
                "arg492"
                (DatatypeT "List" [TypeVarT "b"])
                (FunctionT
                  "arg493"
                  (DatatypeT "List" [TypeVarT "c"])
                  (DatatypeT
                    "List"
                    [ DatatypeT
                        "Pair"
                        [ DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"]
                        , TypeVarT "c"
                        ]
                    ]
                  )
                )
              )
            )
          )
        )
      )
    )
  , ( "GHC.List.zipWith"
    , ForallT
      "a"
      (ForallT
        "b"
        (ForallT
          "c"
          (Monotype
            (FunctionT
              "arg494"
              (FunctionT "arg497"
                         (TypeVarT "a")
                         (FunctionT "arg498" (TypeVarT "b") (TypeVarT "c"))
              )
              (FunctionT
                "arg495"
                (DatatypeT "List" [TypeVarT "a"])
                (FunctionT "arg496"
                           (DatatypeT "List" [TypeVarT "b"])
                           (DatatypeT "List" [TypeVarT "c"])
                )
              )
            )
          )
        )
      )
    )
  , ( "GHC.List.zipWith3"
    , ForallT
      "a"
      (ForallT
        "b"
        (ForallT
          "c"
          (ForallT
            "d"
            (Monotype
              (FunctionT
                "arg499"
                (FunctionT
                  "arg503"
                  (TypeVarT "a")
                  (FunctionT
                    "arg504"
                    (TypeVarT "b")
                    (FunctionT "arg505" (TypeVarT "c") (TypeVarT "d"))
                  )
                )
                (FunctionT
                  "arg500"
                  (DatatypeT "List" [TypeVarT "a"])
                  (FunctionT
                    "arg501"
                    (DatatypeT "List" [TypeVarT "b"])
                    (FunctionT "arg502"
                               (DatatypeT "List" [TypeVarT "c"])
                               (DatatypeT "List" [TypeVarT "d"])
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  , ("Nil", ForallT "a" (Monotype (DatatypeT "List" [TypeVarT "a"])))
  , ( "Pair"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "x"
            (TypeVarT "a")
            (FunctionT "y"
                       (TypeVarT "b")
                       (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
            )
          )
        )
      )
    )
  , ("Prelude.EQ", Monotype (DatatypeT "Ordering" []))
  , ("Prelude.GT", Monotype (DatatypeT "Ordering" []))
  , ("Prelude.LT", Monotype (DatatypeT "Ordering" []))
  , ( "Prelude.compare"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg510"
          (DatatypeT "@@hplusTC@@Ord" [TypeVarT "a"])
          (FunctionT
            "arg508"
            (TypeVarT "a")
            (FunctionT "arg509" (TypeVarT "a") (DatatypeT "Ordering" []))
          )
        )
      )
    )
  , ( "Prelude.max"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg525"
          (DatatypeT "@@hplusTC@@Ord" [TypeVarT "a"])
          (FunctionT "arg523"
                     (TypeVarT "a")
                     (FunctionT "arg524" (TypeVarT "a") (TypeVarT "a"))
          )
        )
      )
    )
  , ( "Prelude.min"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg528"
          (DatatypeT "@@hplusTC@@Ord" [TypeVarT "a"])
          (FunctionT "arg526"
                     (TypeVarT "a")
                     (FunctionT "arg527" (TypeVarT "a") (TypeVarT "a"))
          )
        )
      )
    )
  , ( "Text.Show.show"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg534"
          (DatatypeT "@@hplusTC@@Show" [TypeVarT "a"])
          (FunctionT "arg533"
                     (TypeVarT "a")
                     (DatatypeT "List" [DatatypeT "Char" []])
          )
        )
      )
    )
  , ( "Text.Show.showChar"
    , Monotype
      (FunctionT
        "arg539"
        (DatatypeT "Char" [])
        (FunctionT "arg529"
                   (DatatypeT "List" [DatatypeT "Char" []])
                   (DatatypeT "List" [DatatypeT "Char" []])
        )
      )
    )
  , ( "Text.Show.showList"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg536"
          (DatatypeT "@@hplusTC@@Show" [TypeVarT "a"])
          (FunctionT
            "arg535"
            (DatatypeT "List" [TypeVarT "a"])
            (FunctionT "arg529"
                       (DatatypeT "List" [DatatypeT "Char" []])
                       (DatatypeT "List" [DatatypeT "Char" []])
            )
          )
        )
      )
    )
  , ( "Text.Show.showListWith"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg543"
          (FunctionT
            "arg545"
            (TypeVarT "a")
            (FunctionT "arg529"
                       (DatatypeT "List" [DatatypeT "Char" []])
                       (DatatypeT "List" [DatatypeT "Char" []])
            )
          )
          (FunctionT
            "arg544"
            (DatatypeT "List" [TypeVarT "a"])
            (FunctionT "arg529"
                       (DatatypeT "List" [DatatypeT "Char" []])
                       (DatatypeT "List" [DatatypeT "Char" []])
            )
          )
        )
      )
    )
  , ( "Text.Show.showParen"
    , Monotype
      (FunctionT
        "arg541"
        (DatatypeT "Bool" [])
        (FunctionT
          "arg542"
          (FunctionT "arg529"
                     (DatatypeT "List" [DatatypeT "Char" []])
                     (DatatypeT "List" [DatatypeT "Char" []])
          )
          (FunctionT "arg529"
                     (DatatypeT "List" [DatatypeT "Char" []])
                     (DatatypeT "List" [DatatypeT "Char" []])
          )
        )
      )
    )
  , ( "Text.Show.showString"
    , Monotype
      (FunctionT
        "arg540"
        (DatatypeT "List" [DatatypeT "Char" []])
        (FunctionT "arg529"
                   (DatatypeT "List" [DatatypeT "Char" []])
                   (DatatypeT "List" [DatatypeT "Char" []])
        )
      )
    )
  , ( "Text.Show.shows"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg538"
          (DatatypeT "@@hplusTC@@Show" [TypeVarT "a"])
          (FunctionT
            "arg537"
            (TypeVarT "a")
            (FunctionT "arg529"
                       (DatatypeT "List" [DatatypeT "Char" []])
                       (DatatypeT "List" [DatatypeT "Char" []])
            )
          )
        )
      )
    )
  , ( "Text.Show.showsPrec"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg532"
          (DatatypeT "@@hplusTC@@Show" [TypeVarT "a"])
          (FunctionT
            "arg530"
            (DatatypeT "Int" [])
            (FunctionT
              "arg531"
              (TypeVarT "a")
              (FunctionT "arg529"
                         (DatatypeT "List" [DatatypeT "Char" []])
                         (DatatypeT "List" [DatatypeT "Char" []])
              )
            )
          )
        )
      )
    )
  , ( "fst"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "p"
                     (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
                     (TypeVarT "a")
          )
        )
      )
    )
  , ( "snd"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "p"
                     (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
                     (TypeVarT "b")
          )
        )
      )
    )
  ]

hplusHigherOrders :: [(Text, SchemaSkeleton)]
hplusHigherOrders =
  [ ( "(Data.Eq./=)_0'ho'"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg305"
          (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
          (FunctionT "arg303"
                     (TypeVarT "a")
                     (DatatypeT "Fun" [TypeVarT "a", DatatypeT "Bool" []])
          )
        )
      )
    )
  , ( "(Data.Eq./=)_1'ho'"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg305"
          (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
          (DatatypeT
            "Fun"
            [TypeVarT "a", DatatypeT "Fun" [TypeVarT "a", DatatypeT "Bool" []]]
          )
        )
      )
    )
  , ( "(Data.Eq./=)_2'ho'"
    , ForallT
      "a"
      (Monotype
        (DatatypeT
          "Fun"
          [ DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"]
          , DatatypeT
            "Fun"
            [TypeVarT "a", DatatypeT "Fun" [TypeVarT "a", DatatypeT "Bool" []]]
          ]
        )
      )
    )
  , ( "(Data.Eq.==)_0'ho'"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg302"
          (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
          (FunctionT "arg300"
                     (TypeVarT "a")
                     (DatatypeT "Fun" [TypeVarT "a", DatatypeT "Bool" []])
          )
        )
      )
    )
  , ( "(Data.Eq.==)_1'ho'"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "tcarg302"
          (DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"])
          (DatatypeT
            "Fun"
            [TypeVarT "a", DatatypeT "Fun" [TypeVarT "a", DatatypeT "Bool" []]]
          )
        )
      )
    )
  , ( "(Data.Eq.==)_2'ho'"
    , ForallT
      "a"
      (Monotype
        (DatatypeT
          "Fun"
          [ DatatypeT "@@hplusTC@@Eq" [TypeVarT "a"]
          , DatatypeT
            "Fun"
            [TypeVarT "a", DatatypeT "Fun" [TypeVarT "a", DatatypeT "Bool" []]]
          ]
        )
      )
    )
  , ( "(Data.Function.$)_0'ho'"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT "arg319"
                     (FunctionT "arg321" (TypeVarT "a") (TypeVarT "b"))
                     (DatatypeT "Fun" [TypeVarT "a", TypeVarT "b"])
          )
        )
      )
    )
  , ( "(Data.Function.$)_1'ho'"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (DatatypeT
            "Fun"
            [ DatatypeT "Fun" [TypeVarT "a", TypeVarT "b"]
            , DatatypeT "Fun" [TypeVarT "a", TypeVarT "b"]
            ]
          )
        )
      )
    )
  , ( "Data.Either.Left_0'ho'"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (DatatypeT
            "Fun"
            [TypeVarT "a", DatatypeT "Either" [TypeVarT "a", TypeVarT "b"]]
          )
        )
      )
    )
  , ( "Data.Either.Right_0'ho'"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (DatatypeT
            "Fun"
            [TypeVarT "b", DatatypeT "Either" [TypeVarT "a", TypeVarT "b"]]
          )
        )
      )
    )
  , ( "Data.Function.id_0'ho'"
    , ForallT "a" (Monotype (DatatypeT "Fun" [TypeVarT "a", TypeVarT "a"]))
    )
  , ( "Data.Tuple.snd_0'ho'"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (DatatypeT
            "Fun"
            [DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"], TypeVarT "b"]
          )
        )
      )
    )
  , ( "GHC.List.head_0'ho'"
    , ForallT
      "a"
      (Monotype
        (DatatypeT "Fun" [DatatypeT "List" [TypeVarT "a"], TypeVarT "a"])
      )
    )
  , ( "GHC.List.repeat_0'ho'"
    , ForallT
      "a"
      (Monotype
        (DatatypeT "Fun" [TypeVarT "a", DatatypeT "List" [TypeVarT "a"]])
      )
    )
  , ( "GHC.List.zip_0'ho'"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (FunctionT
            "arg489"
            (DatatypeT "List" [TypeVarT "a"])
            (DatatypeT
              "Fun"
              [ DatatypeT "List" [TypeVarT "b"]
              , DatatypeT "List" [DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"]]
              ]
            )
          )
        )
      )
    )
  , ( "GHC.List.zip_1'ho'"
    , ForallT
      "a"
      (ForallT
        "b"
        (Monotype
          (DatatypeT
            "Fun"
            [ DatatypeT "List" [TypeVarT "a"]
            , DatatypeT
              "Fun"
              [ DatatypeT "List" [TypeVarT "b"]
              , DatatypeT "List" [DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"]]
              ]
            ]
          )
        )
      )
    )
  ]

includedModules :: [Text]
includedModules =
  [ "Data.Int"
  , "Data.Bool"
  , "Data.Maybe"
  , "Data.Either"
  , "Data.Tuple"
  , "Text.Show"
  , "GHC.Char"
  , "GHC.List"
  , "Data.Eq"
  , "Data.List"
  , "Data.Function"
  , "Prelude"
  , "Data.String"
  , "Data.ByteString.Lazy"
  , "Data.ByteString.Builder"
  ]

resolver :: ResolverState
resolver = ResolverState
  { getSynonyms  = [ TypeSynonym
                     "ShowS"
                     []
                     (FunctionT "arg529"
                                (DatatypeT "List" [DatatypeT "Char" []])
                                (DatatypeT "List" [DatatypeT "Char" []])
                     )
                   , TypeSynonym "FilePath"
                                 []
                                 (DatatypeT "List" [DatatypeT "Char" []])
                   , TypeSynonym "String"
                                 []
                                 (DatatypeT "List" [DatatypeT "Char" []])
                   ]
  , getDatatypes = [ DatatypeDef "@@hplusTC@@Show"      ["a"]      []
                   , DatatypeDef "IO"                   ["a"]      []
                   , DatatypeDef "@@hplusTC@@Read"      ["a"]      []
                   , DatatypeDef "@@hplusTC@@Semigroup" ["a"]      []
                   , DatatypeDef "@@hplusTC@@Num"       ["a"]      []
                   , DatatypeDef "@@hplusTC@@Ord"       ["a"]      []
                   , DatatypeDef "@@hplusTC@@IsString"  ["a"]      []
                   , DatatypeDef "Maybe"                ["a"]      []
                   , DatatypeDef "@@hplusTC@@Eq"        ["a"]      []
                   , DatatypeDef "Either"               ["b", "a"] []
                   , DatatypeDef "Fun"                  ["a", "b"] []
                   , DatatypeDef "Word"                 []         []
                   , DatatypeDef "Integer"              []         []
                   , DatatypeDef "Ordering"             []         []
                   , DatatypeDef "Handle"               []         []
                   , DatatypeDef "Int64"                []         []
                   , DatatypeDef "Int32"                []         []
                   , DatatypeDef "Int16"                []         []
                   , DatatypeDef "Int8"                 []         []
                   , DatatypeDef "ByteString"           []         []
                   , DatatypeDef "Word64"               []         []
                   , DatatypeDef "Word32"               []         []
                   , DatatypeDef "Word16"               []         []
                   , DatatypeDef "Word8"                []         []
                   , DatatypeDef "ShortByteString"      []         []
                   , DatatypeDef "Builder"              []         []
                   , DatatypeDef "Double"               []         []
                   , DatatypeDef "Float"                []         []
                   , DatatypeDef "Bool"                 []         []
                   , DatatypeDef "Int"                  []         []
                   , DatatypeDef "Unit"                 []         []
                   , DatatypeDef "Char"                 []         []
                   , DatatypeDef "Pair"                 ["a", "b"] ["Pair"]
                   , DatatypeDef "List" ["a"] ["Nil", "Cons"]
                   ]
  , getIdCount   = 0
  }
