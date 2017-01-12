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

module Orestes.Store where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Data.ByteString.Char8 ( ByteString, pack, unpack )
import Orestes.Parser ( Key, Value )


type Store = TVar ( Map.Map Key Value )

version :: ByteString
version = pack "0.0.1"

notFound :: ByteString
notFound = pack "Not found"


createStore :: IO Store
createStore =
    atomically . newTVar $ Map.singleton (pack "__version__") version


put :: Key -> Value -> Store -> IO ()
put k v store =
    atomically . modifyTVar store $ Map.insert k v


get :: Key -> Store -> IO Value
get k store = do
    db <- atomically $ readTVar store
    return $ Map.findWithDefault notFound k db


del :: Key -> Store -> IO ()
del k store =
    atomically $ modifyTVar store $ Map.delete k
