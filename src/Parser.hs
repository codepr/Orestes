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
Module      :  Parser
Copyright   :  Copyright (c) 2017 Andrea Giacomo Baldan
License     :  MIT

Maintainer  :  a.g.baldan@gmail.com
Stability   :  provisional
Portability :  portable

Parsing command module, handle incoming output from clients and
translates it to Commands

-}


module Parser ( Command ( Put
                        , Get
                        , Del
                        , Info
                        )
              , parseRequest
              , parseCRequest
              ) where

import qualified Messages as M
import Store ( Key, Value )
--
data Command = Put Key Value
             | Get Key
             | Del Key
             | Info
             deriving (Show, Eq)

-- | Parse a request formed by a list of String, returning the correct command
-- or a String "Error" if the command is unknown. Should be handled with a
-- better error system or exceptions.
--
-- FIXME: Ugly, refactor in order to handle ByteString and a custom
-- communication protocol, possibly something trivial
--
parseRequest :: [String] -> Maybe Command
parseRequest request =
    case head request of
      "put"  -> Just $ Put k v where
          k = head $ tail request
          v = unwords $ drop 2 request
      "get"  -> Just $ Get k where
          k = head $ tail request
      "del"  -> Just $ Del k where
          k = head $ tail request
      "info" -> Just Info
      _      -> Nothing

-- | Parse a request formed by a list of String, returning the correct command
-- or a String "Error" if the command is unknown. Should be handled with a
-- better error system or exceptions.
--
-- FIXME: Ugly, refactor in order to handle ByteString and a custom
-- communication protocol, possibly something trivial
--
parseCRequest :: [String] -> Maybe M.Message
parseCRequest request =
    case request of
      ["put", k, v] -> Just $ M.Put Nothing k v
      ["get", k]    -> Just $ M.Get Nothing k
      ["del", k]    -> Just $ M.Del Nothing k
      _             -> Nothing

