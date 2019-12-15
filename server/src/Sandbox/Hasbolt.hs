{-# LANGUAGE OverloadedStrings #-}
module Sandbox.Hasbolt where

import Database.Bolt

import Data.Default
import Data.Text
import Control.Monad
import Control.Monad.Except

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
