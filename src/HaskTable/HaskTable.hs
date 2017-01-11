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

module HaskTable.HaskTable (serveHashMap) where


import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import Control.Exception (SomeException, catch, finally)
import Control.Concurrent.STM
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (forever, unless)
import System.IO (Handle, hClose, hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import HaskTable.Parser ( Command (Put, Get, Del, Info, Echo)
                        , Key
                        , Value
                        , parseRequest)


type Store = Map.Map Key Value

ok :: String
ok = "OK"

notFound :: ByteString
notFound = pack "Not found"

version :: ByteString
version = pack "0.0.1"


serveHashMap :: Integer -> IO ()
serveHashMap port = do
    putStrLn $ "<*> Serving on port " ++ show port
    database <- atomically $ newTVar $ Map.singleton (pack "__version__") version
    serve database $ PortNumber $ fromInteger port


serve :: TVar Store -> PortID -> IO ()
serve store port = withSocketsDo $ do
    sock <- listenOn port
    forever $ wait sock store


wait :: Socket -> TVar Store -> IO ThreadId
wait sock store = do
    (client, _, _) <- accept sock
    putStrLn "<*> Connection received"
    forkIO $ commandProcessor client store


commandProcessor :: Handle -> TVar Store -> IO ()
commandProcessor handle store = do
    line <- hGetLine handle
    let cmd = words line in
        case parseRequest cmd of
          Left err  -> return ()
          Right req -> do
              response <- processWith req store
              hPutStrLn handle $ response
              commandProcessor handle store


processWith :: Command -> TVar Store -> IO String
processWith (Put k v) store = do
    atomically $ modifyTVar store $ Map.insert k v
    return ok

processWith (Get k) store = do
    db <- atomically $ readTVar store
    let x = Map.findWithDefault notFound k db in
        return $ unpack x

processWith (Del k) store = do
    atomically $ modifyTVar store $ Map.delete k
    return ok

processWith (Info) store = processWith (Get $ pack "__version__") store
processWith (Echo r) store = return $ unpack r

