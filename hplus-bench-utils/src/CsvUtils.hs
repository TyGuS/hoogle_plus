{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CsvUtils where


-- base
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- cassava
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Item =
  Item
    { expa      :: Text
    , typeSig   :: Text
    }
  deriving (Eq, Show)

itemHeader :: Header
itemHeader =
  Vector.fromList
    [ "expa"
    , "typeSig"
    ]

instance ToNamedRecord Item where
  toNamedRecord Item {..} =
    Cassava.namedRecord
      [ "expa"     .= expa
      , "typeSig" .= typeSig
      ]

instance DefaultOrdered Item where
  headerOrder _ =
    Cassava.header
      [ "expa"
      , "typeSig"
      ]

encodeItems
  :: Vector Item
  -> ByteString
encodeItems =
  Cassava.encodeDefaultOrderedByName . Foldable.toList


encodeItemsToFile
  :: FilePath
  -> Vector Item
  -> IO ()--(Either String ())
encodeItemsToFile filePath =
  --catchShowIO . ByteString.writeFile filePath . encodeItems
  ByteString.writeFile filePath . encodeItems

catchShowIO :: IO a-> IO (Either String a)
catchShowIO action =
  fmap Right action
    `Exception.catch` handleIOException
  where
    handleIOException
      :: IOException
      -> IO (Either String a)
    handleIOException =
      return . Left . show
