module Database.Presets where

import Types.Generate
import Types.Experiments
import Types.Environment

getOptsFromPreset :: Preset -> GenerationOpts
getOptsFromPreset TotalFunctions = genOptsTier1
getOptsFromPreset PartialFunctions = genOptsTier2

genOptsTier1 = defaultGenerationOpts {
  modules = myModules,
  pkgFetchOpts = Local {
      files = ["libraries/tier1/base.txt", "libraries/tier1/bytestring.txt", "libraries/ghc-prim.txt"]
      }
  }

genOptsTier2 = genOptsTier1 {
  modules = myModules,
  pkgFetchOpts = Local {
      files = ["libraries/base.txt", "libraries/bytestring.txt", "libraries/ghc-prim.txt"]
      }
  }

myModules = [
  -- base
  "Data.Bool",
  "Data.Maybe",
  "Text.Show",
  "GHC.Char",
  "GHC.List",
  "Data.Eq",
  "Data.List",
  "Data.Function"
  ]
