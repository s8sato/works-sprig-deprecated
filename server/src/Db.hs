{-# LANGUAGE OverloadedStrings #-}
module Db where

import Database.Bolt
import Data.Default
import Data.Text
import Control.Monad
import Control.Monad.Except
import Data.Aeson

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Data.Conduit
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Text as T

-- import Database.Bolt (connect, run, query, close, Node(..))
-- import Data.Default (def)

myConfiguration :: BoltCfg
myConfiguration = def { user = "neo4j", password = "Pfg5NlqJ", host = "localhost" }
 
main :: IO ()
main = do pipe <- connect myConfiguration
          records <- run pipe $ query "MATCH (tom {name: 'Tom Hanks'}) RETURN tom"
          let first = Prelude.head records
          cruise <- first `at` "tom" >>= exact :: IO Node
          print cruise
          close pipe


createPerson :: String -> IO ()
createPerson name = do
    pipe <- connect myConfiguration
    result <- run pipe $ query $ pack $ "CREATE (p:Person { name: '" ++ name ++ "' })"
    print result

matchPerson :: String -> IO ()
matchPerson name = do
    pipe <- connect myConfiguration
    records <- run pipe $ query $ pack $ "MATCH (p:Person) WHERE p.name = '" ++ name ++ "' RETURN p;"
    print records


data Person = Person { name :: String }
    deriving (Show, Eq, Ord)

