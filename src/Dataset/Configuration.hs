module Dataset.Configuration where

data Configuration = Configuration {
  repeat :: Int,
  maxArgs :: Int,
  minSize :: Int,
  maxSize :: Int,
  sampleNum :: Int,
  useAbstract :: Bool
}