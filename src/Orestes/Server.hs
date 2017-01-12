{-
Copyright (c) 2017 Andrea Giacomo Baldan

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{- |
Module      :  Orestes.Server
Copyright   :  Copyright (c) 2017 Andrea Giacomo Baldan
License     :  MIT

Maintainer  :  a.g.baldan@gmail.com
Stability   :  provisional
Portability :  portable

Responsible for the networking part and communication with
connecting clients

-}


module Orestes.Server (startServer) where


import Network ( listenOn, withSocketsDo, accept, PortID(..), Socket )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Control.Concurrent ( ThreadId, forkIO )
import Control.Monad ( forever, unless )
import System.IO ( Handle
                   , hSetBuffering
                   , hSetBinaryMode
                   , hGetLine
                   , hPutStrLn
                   , BufferMode( NoBuffering )
                 )

import Data.ByteString.Char8 ( ByteString, pack, unpack )
import Orestes.Parser ( Command ( Put, Get, Del, Info, Echo )
                        , Key
                        , Value
                        , parseRequest )

import Orestes.Store ( Store
                       , version
                       , notFound
                       , createStore
                       , put
                       , get
                       , del
                       )

import Orestes.Commands ( processCommand, ok )


-- | Obtain a PordID from an Integer specifying a port to listen to
onPort :: Integer -> PortID
onPort port = PortNumber $ fromInteger port


-- | Start a server on a specified port, serving the concurrent hashmap by
-- creating a new transactional memory variable
-- Initially the map contains only the version of the program
startServer :: Integer -> IO ()
startServer port = do
    putStrLn $ "<*> Listening on localhost:" ++ show port
    database <- createStore
    serve database $ onPort port


-- | Listen on localhost at the specified port
serve :: Store -> PortID -> IO ()
serve store port = withSocketsDo $ do
    sock <- listenOn port
    forever $ wait sock store


-- | Wait for incoming connection and spawn a new thread on every successful one
-- passing responsibilities of command handling to it
wait :: Socket -> Store -> IO ThreadId
wait sock store = do
    (client, _, _) <- accept sock
    hSetBuffering client NoBuffering
    hSetBinaryMode client True
    putStrLn "<*> Connection received"
    forkIO $ commandProcessor client store
    wait sock store


-- | Parse command by processing every incoming String from the client, sendig
-- back response based on the operation requested.
-- Currently handle put, get, del, info and echo.
commandProcessor :: Handle -> Store -> IO ()
commandProcessor handle store = do
    line <- hGetLine handle
    let cmd = words line in
        case parseRequest cmd of
          Left err  -> do
              hPutStrLn handle $ err
              commandProcessor handle store
          Right cmd -> do
              response <- processCommand cmd store
              hPutStrLn handle $ response
              commandProcessor handle store

