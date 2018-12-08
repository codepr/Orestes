{-
Copyright (c) 2016-2018 Andrea Giacomo Baldan

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

module CommandsSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Commands ( processCommand, Reply(..) )
import Parser ( Command ( Info, Put, Get, Del ) )
import Store ( createStore )


spec :: Spec
spec = do
    describe "Commands" $ do

        it "should process an info command" $ do
            let store = createStore
            ans <- processCommand Info store
            ans `shouldBe` (Reply store "0.2.0.0")

        it "should process a put command" $ do
            let store = createStore
            ans <- processCommand (Put "key" "value") store
            let expected = Map.fromList [("__version__", "0.2.0.0"), ("key", "value")] in
                ans `shouldBe` (Reply expected "OK")

        it "should process a get command" $ do
            let store = createStore
            -- FIXME: use a real content
            ans <- processCommand (Get "__version__") store
            ans `shouldBe` (Reply store "0.2.0.0")

        it "should process a del command" $ do
            let store = createStore
            ans <- processCommand (Del "__version__") store
            let expected = Map.empty in
                ans `shouldBe` (Reply expected "OK")
