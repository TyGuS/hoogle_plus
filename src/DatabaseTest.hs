{-# LANGUAGE ScopedTypeVariables #-}

import Database.Generate
import Database.Convert
import Database.Download
import Database.Graph
import Synquid.Type
import Synquid.Pretty

convertTest = printDeclarations "bytestring" (Just "0.10.8.1")

downloadTest = do
    downloadFile "bytestring" (Just "0.10.8.1")
    downloadFile "bytestring" Nothing

main = do
    convertTest
    -- downloadTest
    