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
Module      :  Orestes.Parser
Copyright   :  Copyright (c) 2017 Andrea Giacomo Baldan
License     :  MIT

Maintainer  :  a.g.baldan@gmail.com
Stability   :  provisional
Portability :  portable

Parsing command module, handle incoming output from clients and
translates it to Commands

-}


module Orestes.Parser( Command ( Put, Get, Del, Info, Echo )
                       , Key
                       , Value
                       , parseRequest
                       ) where

import Data.ByteString.Char8 ( ByteString, pack )

type Key = ByteString

type Value = ByteString

data Command = Put Key Value
             | Get Key
             | Del Key
             | Info
             | Echo String
             deriving (Show, Eq)

-- | Parse a request formed by a list of String, returning the correct command
-- or a String "Error" if the command is unknown. Should be handled with a
-- better error system or exceptions.
--
-- FIXME: Ugly, refactor in order to handle ByteString and a custom
-- communication protocol, possibly something trivial
--
parseRequest :: [String] -> Either String Command
parseRequest request =
    case (head request) of
      ("put")  -> Right (Put k v) where
          k = pack . head $ tail request
          v = pack . unwords $ drop 2 request
      ("get")  -> Right (Get k) where
          k = pack . head $ tail request
      ("del")  -> Right (Del k) where
          k = pack . head $ tail request
      ("echo") -> Right (Echo . unwords $ tail request)
      ("info") -> Right (Info)
      _        -> Left ("Unknown command")
