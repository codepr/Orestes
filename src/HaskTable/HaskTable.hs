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

module HaskTable.HaskTable (startServer) where


import Network ( listenOn, withSocketsDo, accept, PortID(..), Socket )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Control.Concurrent ( ThreadId, forkIO, threadDelay )
import Control.Monad ( forever, unless )
import System.IO ( Handle, hSetBuffering, hSetBinaryMode, hGetLine, hPutStrLn, BufferMode(..) )
import Data.ByteString.Char8 ( ByteString, pack, unpack )
import HaskTable.Parser ( Command ( Put, Get, Del, Info, Echo )
                        , Key
                        , Value
                        , parseRequest )

import HaskTable.Store ( Store
                       , createStore
                       , put
                       , get
                       , del
                       )

-- type Store = Map.Map Key Value

ok :: String
ok = "OK"

notFound :: ByteString
notFound = pack "Not found"

version :: ByteString
version = pack "0.0.1"


-- | Obtain a PordID from an Integer specifying a port to listen to
onPort :: Integer -> PortID
onPort port = PortNumber $ fromInteger port


-- | Start a server on a specified port, serving the concurrent hashmap by
-- creating a new transactional memory variable
-- Initially the map contains only the version of the program
startServer :: Integer -> IO ()
startServer port = do
    putStrLn $ "<*> Listening on localhost:" ++ show port
    -- database <- atomically $ newTVar $ Map.singleton (pack "__version__") version
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
          Left err  -> return ()
          Right cmd -> do
              response <- processCommand cmd store
              hPutStrLn handle $ response
              commandProcessor handle store


-- | Process a command and return an IO Monad containing a String representing
-- the result of the operation, or an Ack/Nack based on the success of the
-- request
processCommand :: Command -> Store -> IO String
processCommand (Put k v) store = do
    -- atomically $ modifyTVar store $ Map.insert k v
    put k v store
    return ok

processCommand (Get k) store = do
    -- db <- atomically $ readTVar store
    -- let x = Map.findWithDefault notFound k db in
    x <- get k store
    return $ unpack x

processCommand (Del k) store = do
    -- atomically $ modifyTVar store $ Map.delete k
    del k store
    return ok

processCommand (Info) store = processCommand (Get $ pack "__version__") store
processCommand (Echo r) store = return $ r

