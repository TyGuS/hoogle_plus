module Database.Presets where

import Types.Generate

getOptsFromPreset :: Preset -> GenerationOpts
getOptsFromPreset TotalFunctions = genOptsTier1
getOptsFromPreset PartialFunctions = genOptsTier2
getOptsFromPreset ECTAFull = genOptsECTA
getOptsFromPreset ECTAPartial = genOptsECTA2

genOptsTier1 :: GenerationOpts
genOptsTier1 = defaultGenerationOpts {
  modules = myModules,
  pkgFetchOpts = Local {
      files = ["libraries/tier1/base.txt", "libraries/tier1/bytestring.txt", "libraries/ghc-prim.txt"]
      }
  }

genOptsTier2 :: GenerationOpts
genOptsTier2 = genOptsTier1 {
  modules = myModules,
  pkgFetchOpts = Local {
      files = ["libraries/base.txt", "libraries/bytestring.txt", "libraries/ghc-prim.txt"]
      }
  }

genOptsECTA :: GenerationOpts
genOptsECTA = defaultGenerationOpts {
  modules = ectaModules,
  pkgFetchOpts = Local {
    files = ["libraries/base_ecta.txt", "libraries/bytestring.txt", "libraries/ghc-prim.txt"]
    },
  hoOption = HOFAll
  }

genOptsECTA2 :: GenerationOpts
genOptsECTA2 = genOptsECTA {
  hoOption = HOFPartial
  }

myModules :: [String]
myModules = [
  -- base
  "Data.Int",
  "Data.Bool",
  "Data.Maybe",
  "Data.Either",
  "Data.Tuple",
  "Text.Show",
  "GHC.Char",
  "GHC.List",
  "Data.Eq",
  "Data.List",
  "Data.Function",
  -- ByteString
  "Data.ByteString.Lazy",
  "Data.ByteString.Builder"
  ]

ectaModules :: [String]
ectaModules = [
  -- base
  "Data.Int",
  "Data.Bool",
  "Data.Maybe",
  "Data.Either",
  "Data.Tuple",
  "Text.Show",
  "GHC.Char",
  "GHC.List",
  "Data.Eq",
  "Data.Ord",
  "Data.List",
  "Data.Function",
  -- ByteString
  "Data.ByteString.Lazy",
  "Data.ByteString.Builder"
  ]
