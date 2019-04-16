module BConfig where

import Types.Generate
import Types.Experiments
import Types.Environment
import BTypes

searchParams = defaultSearchParams
searchParamsHOF = defaultSearchParams{_useHO=True}
searchParamsBaseline = defaultSearchParams{_useRefine=NoRefine}
searchParamsZeroStart = defaultSearchParams{_useRefine=AbstractRefinement}

genOptsTier1 = defaultGenerationOpts {
  modules = myModules,
  pkgFetchOpts = Local {
      files = ["libraries/tier1/base.txt", "libraries/tier1/bytestring.txt", "libraries/ghc-prim.txt"]
      }
  }

genOptsTier2 = genOptsTier1 {
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
  -- ByteString
  "Data.ByteString.Lazy",
  "Data.ByteString.Builder"
  ]
