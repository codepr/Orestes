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

module HaskTable.Parser( Request(Put, Get, Del, Empty, Echo)
                       , Reply (store, content)
                       , parseRequest
                       ) where

import qualified Data.Map as Map

data Request = Put String [String]
             | Get String
             | Del String
             | Empty
             | Echo [String] deriving (Show, Eq)

data Reply = Reply { store :: Map.Map String [String]
                   , content :: String } deriving (Show)

-- | Parse a request formed by a list of String, returning the correct command
-- or an error if the command is unknown
parseRequest :: [String] -> Either String Request
parseRequest request =
    case (head request) of
      ("put")   -> Right (Put (head (tail request)) (drop 2 request))
      ("get")   -> Right (Get $ head (tail request))
      ("del")   -> Right (Del $ head (tail request))
      ("echo")  -> Right (Echo $ tail request)
      ("empty") -> Right (Empty)
      _ -> Left ("Error")
