{-# LANGUAGE TemplateHaskell, DeriveGeneric#-}
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
Module      :  InitDB
Copyright   :  Copyright (c) 2017 Andrea Giacomo Baldan
License     :  MIT

Maintainer  :  a.g.baldan@gmail.com
Stability   :  provisional
Portability :  portable

Initialization of the store across the cluster slave nodes

-}


module InitDB where


import Control.Distributed.Process hiding ( Message )
import Control.Distributed.Process.Closure
import Store ( createStore, put, get, del )
import Commands ( ok )
import Messages


-- | Initialization function, called by a static closure by the master node
-- in a dedicated process on every slave node
-- Spawn a recursive loop that process incoming messages from other process
-- and perform operations on the local piece of the store
--
initDatabase :: Process ()
initDatabase = loop createStore where
    loop store = do
        msg <- expect
        case msg of
          Put s k v -> do
              case s of
                Nothing     -> say "[NO SENDER]"
                Just sender -> do
                    say $ "[PUT] " ++ k ++ " -> " ++ v
                    send sender $ Just ok
              loop $ put k v store
          Get s k -> do
              case s of
                Nothing     -> say "[NO SENDER]"
                Just sender -> do
                    say $ "[GET] " ++ k
                    send sender $ Just (get k store)
              loop store
          Del s k -> do
              case s of
                Nothing     -> say "[NO SENDER]"
                Just sender -> do
                    say $ "[DEL] " ++ k
                    send sender $ Just ok
              loop $ del k store
          _ -> loop store


remotable ['initDatabase]


