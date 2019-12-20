module Util where

import System.IO
import System.Directory
import Synquid.Util

endFile :: FilePath -> Handle -> IO ()
endFile file hdl = do
    hClose hdl
    ifM (doesFileExist file) (removeFile file) (return ())
