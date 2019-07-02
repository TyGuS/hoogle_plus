module Database.Presets where

import Types.Generate
import Types.Experiments
import Types.Environment

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
  "Data.Int",
  "Data.Bool",
  "Data.Maybe",
  "Data.Either",
  "Data.Tuple",
  "GHC.Char",
  "Text.Show",
  "GHC.List",
  "Data.Function",
  -- ByteString
  "Data.ByteString.Lazy",
  "Data.ByteString.Builder"
  ]
