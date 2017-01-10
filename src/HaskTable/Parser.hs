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

-- Parse a request formed by a list of String, returning the correct command
parseRequest :: [String] -> Either String Request
parseRequest request =
    case (head request) of
      ("put") -> Right (Put (head (tail request)) (drop 2 request))
      ("get") -> Right (Get $ head (tail request))
      ("del") -> Right (Del $ head (tail request))
      ("echo") -> Right (Echo $ tail request)
      ("empty") -> Right (Empty)
      _ -> Left ("Error")
