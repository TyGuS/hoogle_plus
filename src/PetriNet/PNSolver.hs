{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}

module PetriNet.PNSolver where

import Data.Text (Text)
import Language.Java
import Language.Java.Inline

test :: IO ()
test = withJVM [] $ do
    message <- reflect ("Hello World!" :: Text)
    [java| {
      javax.swing.JOptionPane.showMessageDialog(null, $message);
      } |]