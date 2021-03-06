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


module ParserSpec where

import Test.Hspec
import Parser ( parseRequest
              , Command ( Info
                        , Put
                        , Get
                        , Del
                        )
              )

spec :: Spec
spec = do
    describe "Parser" $ do
        it "should parse an unknown request" $ do
            parseRequest ["unknowncmd"] `shouldBe` Nothing

        it "should parse a well formed info request" $ do
            parseRequest ["info"] `shouldBe` Just Info

        it "should parse a well formed put request" $ do
            parseRequest ["put", "key", "value"]
            `shouldBe` Just ( Put "key" "value" )

        it "should parse a well formed get request" $ do
            parseRequest ["get", "key"]
            `shouldBe` Just ( Get "key" )

        it "should parse a well formed del request" $ do
            parseRequest ["del", "key"]
            `shouldBe` Just ( Del "key" )

