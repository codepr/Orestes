{-# LANGUAGE TemplateHaskell #-}
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

Author      :  Andrea Giacomo Baldan
Copyright   :  Copyright (c) 2017 Andrea Giacomo Baldan
License     :  MIT

Simple implementation of a distributed key-value server, aimed to learn basic
concepts of functional programming through Haskell. For the distribution part
it uses cloud-haskell libraries, based on asynchronous message protocol like
Erlang distribution actor model. It works on both a cluster of machines or on
a single one according to a master-slave topology.
It currently support just the common operations `PUT`, `GET` and `DEL` and
it lacks a suitable communication protocol, on the other side it is perfectly
usable by using a generic TCP client like Telnet or Netcat issuing commands
as strings.


-}

module Main where


import System.Environment ( getArgs )
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, LocalNode, localNodeId)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad ( forM )
import Server (startServer )
import Cluster ( startCluster )
import Store
import InitDB


-- | Exporting remote table for the initialization of the store across the
-- cluster
orestesRemoteTable :: RemoteTable
orestesRemoteTable = InitDB.__remoteTable initRemoteTable


-- | Initialize the clustered store by spawning a remote process on every
-- slave node of the cluster
createDB :: [NodeId] -> Process [ProcessId]
createDB nodes =
    forM nodes $ \node -> spawn node $ $(mkStaticClosure 'initDatabase)


-- | Define master node behavior and main entry point for the incoming
-- connections
master :: Integer -> LocalNode -> Backend -> [NodeId] -> Process ()
master port node backend slaves = do
    liftIO $ putStrLn $ "<*> Master node started on " ++ show (localNodeId node)
    let cluster = slaves ++ [localNodeId node]
    liftIO $ putStrLn $ "<*> Discovered " ++ show (length slaves) ++ " slaves "++ show slaves
    db <- createDB cluster
    startCluster node db port
    terminateAllSlaves backend


main :: IO ()
main = do
    args <- getArgs
    let database = createStore in
        case args of
          ["master", host, backendPort, port] -> do
              backend <- initializeBackend host backendPort orestesRemoteTable
              node <- newLocalNode backend
              startMaster backend $ master (read port) node backend
          ["master", host, backendPort] -> do
              backend <- initializeBackend host backendPort orestesRemoteTable
              node <- newLocalNode backend
              startMaster backend $ master 6373 node backend
          ["slave", host, backendPort] -> do
              backend <- initializeBackend host backendPort orestesRemoteTable
              startSlave backend
          [port] -> startServer database $ read port
          _ -> startServer database 6373
