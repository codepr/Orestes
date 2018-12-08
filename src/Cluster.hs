{-# OPTIONS_GHC -Wall #-}
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
Module      :  Cluster
Copyright   :  Copyright (c) 2017 Andrea Giacomo Baldan
License     :  MIT

Maintainer  :  a.g.baldan@gmail.com
Stability   :  provisional
Portability :  portable

Responsible for the networking part and communication with
connecting clients in distributed context

-}

module Cluster where


import Network ( listenOn, withSocketsDo, accept, PortID(..), Socket )
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Node
import Control.Concurrent ( ThreadId, forkIO )
import Control.Monad ( forever )
import System.IO ( Handle
                 , hSetBuffering
                 , hSetBinaryMode
                 , hGetLine
                 , hPutStrLn
                 , BufferMode( NoBuffering )
                 )

import Parser ( parseCRequest )
import Commands ( processClusterCommand )
import Server ( onPort )
import Store ( Store, createStore )


-- | Start a server on the master node of the cluster, ready to accept client
-- connections and commands
startCluster :: LocalNode -> [ProcessId] -> Integer -> Process ()
startCluster node pids port = do
    liftIO $ putStrLn $ "<*> Cluster started, TCP server listening on " ++ show port
    let store = createStore in
        liftIO $ serve node pids store $ onPort port


-- | Listen on localhost at the specified port
serve :: LocalNode -> [ProcessId] -> Store -> PortID -> IO ()
serve node pids store port = withSocketsDo $ do
    sock <- listenOn port
    forever $ wait node pids sock store


-- | Wait for incoming connection and spawn a new thread on every successful one
-- passing responsibilities of command handling to it
wait :: LocalNode -> [ProcessId] -> Socket -> Store -> IO ThreadId
wait node pids sock store = do
    (client, _, _) <- accept sock
    hSetBuffering client NoBuffering
    hSetBinaryMode client True
    putStrLn "<*> Connection received"
    _ <- forkIO $ commandProcessor node pids client store
    wait node pids sock store


-- | Parse command by processing every incoming String from the client, sending
-- back response based on the operation requested.
-- Currently handle put, get, del, info and echo.
commandProcessor :: LocalNode -> [ProcessId] -> Handle -> Store -> IO ()
commandProcessor node pids handle store = do
    line <- hGetLine handle
    case parseCRequest $ words line of
      Just cmd -> do
          runProcess node $
              processClusterCommand cmd handle pids
          commandProcessor node pids handle store
      Nothing -> do
          liftIO $ hPutStrLn handle "Unknown command"
          commandProcessor node pids handle store
