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
Module      :  Orestes.Commands
Copyright   :  Copyright (c) 2017 Andrea Giacomo Baldan
License     :  MIT

Maintainer  :  a.g.baldan@gmail.com
Stability   :  provisional
Portability :  portable

Define commands that the store can receive and the returning values

-}


module Orestes.Commands ( processCommand, ok )
    where

import Data.ByteString.Char8 ( ByteString, pack, unpack )
import Orestes.Store ( Store, put, get, del )
import Orestes.Parser ( Command ( Put, Get, Del, Info, Echo ) )


ok :: String
ok = "OK"


-- | Process a command and return an IO Monad containing a String representing
-- the result of the operation, or an Ack/Nack based on the success of the
-- request
--
-- FIXME: Refactor in order to handle directly ByteString using a custom
-- communication protocol
--
processCommand :: Command -> Store -> IO String
processCommand (Put k v) store = do
    put k v store
    return ok

processCommand (Get k) store = do
    x <- get k store
    return $ unpack x  -- ByteString to String

processCommand (Del k) store = do
    del k store
    return ok

processCommand (Info) store = processCommand (Get $ pack "__version__") store
processCommand (Echo r) store = return r

