module Postfilter.GHCSocket (
    askGhcSocket
  ) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Text.Read ( readMaybe )

host :: String
host = "127.0.0.1"

port :: String
port = "1234"

askGhcSocket :: String -> IO (Either String String)
askGhcSocket query = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close $ \s -> do
    sendAll s (C.pack query)
    msg <- recv s 999999999
    -- print $ "client received: " ++ C.unpack msg
    case readMaybe (C.unpack msg) of
      Nothing -> error "invalid response"
      Just x -> return x
  where
    resolve = do
      let hints = defaultHints { addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    
    open addr = E.bracketOnError (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock