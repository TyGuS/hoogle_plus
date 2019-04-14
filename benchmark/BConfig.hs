module BConfig where

import Types.Generate
import Types.Experiments
import Types.Environment

queryFile = "scripts/curated.json"
defaultTimeoutus :: Int
defaultTimeoutus = (1 * 60) * (10 ^ 6) -- 5 minutes in microseconds
searchParams = defaultSearchParams
searchParamsHOF = defaultSearchParams{_useHO=True}
searchParamsBaseline = defaultSearchParams{_useRefine=NoRefine}
searchParamsZeroStart = defaultSearchParams{_useRefine=AbstractRefinement}
searchParamsZeroHOF = searchParamsHOF{_useRefine=AbstractRefinement}

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

genOptsMicro = defaultGenerationOpts {
  modules = myModules,
  pkgFetchOpts = Local {
      files = ["libraries/testHOF1.txt"]
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
