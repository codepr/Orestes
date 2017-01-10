module HaskTable.HaskTable (serveHashMap) where


import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import Control.Exception (SomeException, catch, finally)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (forever, unless)
import System.IO (Handle, hClose, hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Data.ByteString as BS (ByteString, hGet, length, hPut, append)
import HaskTable.Parser (Request (Put, Get, Del, Empty, Echo), Reply (store, content), parseRequest)


type Store = Map.Map String [String]


serveHashMap :: Integer -> IO ()
serveHashMap port = do
    putStrLn $ "<*> Serving on port " ++ show port
    let globalMap = Map.empty in
        serve globalMap $ PortNumber $ fromInteger port


serve :: Store -> PortID -> IO ()
serve store port = withSocketsDo $ do
    sock <- listenOn port
    forever $ wait sock store


wait :: Socket -> Store -> IO ThreadId
wait sock store = do
    (client, _, _) <- accept sock
    putStrLn "Connection received"
    forkIO $ commandProcessor client store
    wait sock store


commandProcessor :: Handle -> Store -> IO ()
commandProcessor handle store = do
    line <- hGetLine handle
    let cmd = words line in
        case parseRequest cmd of
          Left err -> return ()
          Right req -> do
              case processWith req store of
                Left reply -> do
                    hPutStrLn handle $ unwords reply
                    commandProcessor handle store
                Right hmap -> do
                    hPutStrLn handle $ "OK"
                    commandProcessor handle hmap


processWith :: Request -> Store -> Either [String] Store
processWith (Put k v) store =
    Right (Map.insert k v store)

processWith (Get k) store =
    Left(val) where
        val = case Map.lookup k store of
                Just val -> val
                Nothing -> ["Not found"]

processWith (Del k) store =
    Right (Map.delete k store)

processWith (Empty) store =
    Right (Map.empty)

processWith (Echo r) store =
    Left (r)

