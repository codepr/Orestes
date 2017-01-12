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

module Orestes.CommandsSpec where

import Test.Hspec
import Control.Concurrent.STM
import Data.ByteString.Char8 ( pack )
import qualified Data.Map.Strict as Map
import Orestes.Commands ( processCommand )
import Orestes.Parser ( Command ( Echo, Info, Put, Get, Del ) )
import Orestes.Store ( createStore )


spec :: Spec
spec = do
    describe "Commands" $ do
        it "should process an echo command" $ do
            store <- createStore
            ans   <- processCommand (Echo "hello world") store
            ans `shouldBe` "hello world"

        it "should process an echo command" $ do
            store <- createStore
            ans   <- processCommand Info store
            ans `shouldBe` "0.0.1"

        it "should process a put command" $ do
            store <- createStore
            ans   <- processCommand (Put (pack "key") (pack "value")) store
            ans `shouldBe` "OK"
            let expected = Map.fromList [((pack "__version__"), (pack "0.0.1")), ((pack "key"), (pack "value"))] in do
                content <- atomically $ readTVar $ store
                content `shouldBe` expected

        it "should process a get command" $ do
            store <- createStore
            -- FIXME: use a real content
            ans   <- processCommand (Get (pack "__version__")) store
            ans `shouldBe` "0.0.1"

        it "should process a del command" $ do
            store <- createStore
            ans   <- processCommand (Del (pack "__version__")) store
            ans `shouldBe` "OK"
            let expected = Map.empty in do
                content <- atomically $ readTVar $ store
                content `shouldBe` expected

