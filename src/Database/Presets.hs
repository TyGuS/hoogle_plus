module Database.Presets where

import Types.Generate
import Types.Experiments
import Types.Environment

getOptsFromPreset :: Preset -> GenerationOpts
getOptsFromPreset ICFPTotal = genOptsTier1
getOptsFromPreset ICFPPartial = genOptsTier2
getOptsFromPreset POPL = poplWithTypeclasses

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

poplWithTypeclasses = defaultGenerationOpts {
  modules = poplModules,
  pkgFetchOpts = Local {
    files = ["libraries/customPrelude.txt", "libraries/ghc-prim.txt",
             "libraries/containers.txt"]
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
  -- ByteString
  "Data.ByteString.Lazy",
  "Data.ByteString.Builder"
  ]

poplModules = [
  "Prelude", -- This prelude in customPrelude is missing those with HK-tyvars and the zip >=3 family functions.
  "Data.List", -- This is Data.OldList to avoid a Foldable fiasco. We don't support those higher-kinded tyvars yet.
  "Data.Maybe", "Data.Either",
  "Data.Map.Strict", "Data.Set"]