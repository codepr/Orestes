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
Module      :  Store
Copyright   :  Copyright (c) 2017 Andrea Giacomo Baldan
License     :  MIT

Maintainer  :  a.g.baldan@gmail.com
Stability   :  provisional
Portability :  portable

Define the behaviour of the Store, simplyfing common operations that can be
executed on a Map

-}


module Store where


import qualified Data.Map.Strict as Map


type Key   = String
type Value = String
type Store = Map.Map Key Value

version :: String
version = "0.2.0.0"

notFound :: String
notFound = "Not found"


-- | Create a Transactional Map ByteString ByteString
createStore :: Store
createStore = Map.singleton "__version__" version


-- | Insert a key value pair or update it if already present
put :: Key -> Value -> Store -> Store
put k v store =
    Map.insert k v store


-- | Retrieve the value of a key, fallback to "Not found" if the key is not
-- present in the Store
get :: Key -> Store -> Value
get k store = do
    Map.findWithDefault notFound k store


-- | Remove a key value pair, the Store is left untouched if the key is not
-- found
del :: Key -> Store -> Store
del k store =
    Map.delete k store
