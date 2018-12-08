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
Module      :  Commands
Copyright   :  Copyright (c) 2017 Andrea Giacomo Baldan
License     :  MIT

Maintainer  :  a.g.baldan@gmail.com
Stability   :  provisional
Portability :  portable

Define commands that the store can receive and the returning values, in a
distributed context, handles messages incoming from a connected client and
map them to the correct node based on his keyspace.

A simple hashing routing table define every node's keyspace:

put k v => hash k mod (cluster size)

-}


module Commands ( processCommand
                , processClusterCommand
                , ok
                , Reply(..)
                , datastore
                , value
                ) where


import System.IO ( Handle , hPutStrLn )
import Data.Hashable
import Control.Distributed.Process hiding (Message)
import Control.Monad ( forever )
import Store ( Store, put, get, del, notFound, version )
import Parser ( Command ( Put, Get, Del, Info ) )
import qualified Messages as M
--
--
ok :: String
ok = "OK"


data Reply = Reply Store String deriving (Show, Eq)


datastore :: Reply -> Store
datastore (Reply store _) = store


value :: Reply -> String
value (Reply _ val) = val


-- | Process a command and return an IO Monad containing a String representing
-- the result of the operation, or an Ack/Nack based on the success of the
-- request
--
processCommand :: Command -> Store -> IO Reply
processCommand cmd store = do
    case cmd of
      Put k v -> return $ Reply (put k v store) ok
      Get k   -> return $ Reply store $ get k store
      Del k   -> return $ Reply (del k store) ok
      Info    -> return $ Reply store version


-- | Calculate the correct index to route the message across the cluster,
-- resulting from the hash of the key mod nr of nodes
--
getIdx :: String -> Int -> Int
getIdx key len =
    hsh `mod` len where
        hsh = hash key


-- | Route the message to the correct node in the cluster based on the
-- his handled keyspace
-- FIXME repeated code
--
routeMessage :: M.Message -> [ProcessId] -> Handle -> M.Sender -> Process ()
routeMessage message pids handle me =
    case message of
      M.Put _ k v ->
          send pid $ M.Put me k v where
              pid = pids !! (getIdx k $ length pids)
      M.Get _ k   ->
          send pid $ M.Get me k where
              pid = pids !! (getIdx k $ length pids)
      M.Del _ k   ->
          send pid $ M.Del me k where
              pid = pids !! (getIdx k $ length pids)
      M.Info ->
          -- FIXME Add more useful informations, e.g. node stats etc
          liftIO $ hPutStrLn handle version


-- | Process a command in a distributed context, sending the message to the
-- correct process across the system
--
processClusterCommand :: M.Message -> Handle -> [ProcessId] -> Process ()
processClusterCommand message handle pids = do
    -- spawn a local process in order to handle answers from other nodes
    self <- spawnLocal $ forever $ do
        m <- expect
        case m of
          Nothing  -> liftIO $ hPutStrLn handle notFound
          Just ans -> liftIO $ hPutStrLn handle $ ans
    -- route message to the correct process (actor) inside the cluster
    routeMessage message pids handle $ Just self
