{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Database.GraphWeightsProvider
( getGraphWeights
, getGraphWeight
) where

import Data.Aeson
import Data.Typeable
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative
import Control.Monad
import GHC.Generics
import Data.Maybe
import qualified Data.ByteString.Lazy as B


getJSON :: IO B.ByteString
getJSON = B.readFile "./src/Database/bytestring_after_1000_negativeLogProbs.json"

getGraphWeight :: String -> IO Double
getGraphWeight name = do
    -- Get JSON data and decode it
    decodeResult <- (eitherDecode <$> getJSON) :: IO (Either String Value)
    -- If d is Left, the JSON was malformed.
    -- In that case, we report the error.
    -- Otherwise, we perform the operation of
    -- our choice. In this case, just print it.
    case decodeResult of
        Left err -> error err
        Right value -> case value of
          Object contents -> return $ toFloat $ flip HashMap.lookup contents $ Text.pack name
          _ -> error "failure!"
  where
    toFloat v = case v of
        Just (Number num) -> read (show num) :: Double
        Just (String str) -> read (Text.unpack str) :: Double
        Nothing -> 0.01 :: Double -- TODO: the default value here should be changed
        _ -> error $ "Cannot decode as string text when trying" ++ show v

getGraphWeights :: [String] -> IO [Double]
getGraphWeights list = mapM getGraphWeight list