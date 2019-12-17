{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

-- {-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Prelude hiding (take, drop)
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
-- CORS
import Network.Wai.Middleware.Cors 

-- json
import Data.Aeson
import Data.Text hiding (map, filter, reverse, zip)
import Ch10.Builder (Person(..), Client(..))
-- for jsonTo_
import Data.Aeson.Types
import Control.Applicative
-- for jsonToClient
import qualified Data.HashMap.Strict as M

-- parse
import Control.Applicative
-- import Data.Attoparsec.Text
import Ch10.Builder (Person(..), Client(..), clientToText)
-- for parseClients
import Data.Attoparsec.Combinator
-- for loadClients
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.Text as T

-- build
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Conduit.List as L

import Data.Time

-- data User = User
--   { userId        :: Int
--   , userFirstName :: String
--   , userLastName  :: String
--   } deriving (Eq, Show)

yea :: Int
yea = 1
qua :: Int
qua = 4
mon :: Int
mon = 12
wee :: Int
wee = 52
day :: Int
day = 365
hou :: Int
hou = 8760
min :: Int
min = 525600
sec :: Int
sec = 31536000

data DateTime = DateTime
    { date :: String -- "YYYY/MM/DD"
    , time :: String -- "HH:MM'SS"
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''DateTime)

data Bar = Bar
    { dot :: Int
    , sharp :: Int
    , exclamation :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Bar)

data Task = Task
    { isDone' :: Bool
    , isStarred' :: Bool
    , title' :: String
    , start' :: DateTime
    , deadline' :: DateTime
    , weight' :: Int
    , bar' :: Bar
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Task)

data Model = Model
    { scale :: Int
    , tasks :: [Task]
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Model)

type API = "model" :> Get '[JSON] Model

startApp :: IO ()
startApp = run 8080 app

app :: Application
-- app = serve api server
app = simpleCors $ serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return model

model :: Model
model = Model day
        [ Task False True "curry" (DateTime "2019/12/15" "06:35'00") (DateTime "2019/12/16" "06:36:00") 30 (Bar 12 15 16)
        , Task True False "hayasi" (DateTime "2019/12/17" "06:37'00") (DateTime "2019/12/18" "06:38:00") 60 (Bar 9 15 16)
        ]

-- 

type Graph a = [Edge a]
type Edge a = ((Node, Node), a)
type Node = Int
data Attr = Attr
    { taskId :: Int
    , isDone :: Bool
    , isStarred :: Bool
    , title :: Text
    , startDate :: Day
    , startTime :: ZonedTime
    , deadlineDate :: Day
    , deadlineTime :: ZonedTime
    , weight :: Int
    , link :: Text
    }

-- text2graph :: Text -> Parser (Graph Attr)
-- text2graph = assemble . markUp . chopLines

type Indent = Int
indent :: Text
indent = "    "

chopLines :: Text -> [(Indent, [Text])]
chopLines t =  map takeIndent $ filter (/= "") $ splitOn "\n" t

takeIndent :: Text -> (Indent, [Text])
takeIndent = takeIndent' 0

takeIndent' :: Int -> Text -> (Indent, [Text])
takeIndent' c t
    | take l t == indent    = takeIndent' (c + 1) (drop l t)
    | otherwise             = (c, filter (/= "") $ splitOn " " t)
    where l = Data.Text.length indent

markUp :: [(Indent, [Text])] -> [((Node, Node), [Text])]
markUp xs = zip (markUp' $ map fst xs) (map snd xs)

markUp' :: [Indent] -> [(Node, Node)]
markUp' is =  reverse $ markUp'' 0 0 (reverse is) []

markUp'' :: Int -> Int -> [Indent] -> [(Node, Node)] -> [(Node, Node)]
markUp'' sbj obj is mem
    | sbj == Prelude.length is  = mem
    | obj == Prelude.length is  = markUp'' (sbj + 1) (sbj + 1) is ((-1,sbj):mem)
    | is !! sbj > is !! obj     = markUp'' (sbj + 1) (sbj + 1) is ((obj,sbj):mem)
    | otherwise                 = markUp'' sbj (obj + 1) is mem

-- assemble :: [((Node, Node), [Text])] -> Parser (Graph Attr)


-- graph2text :: Graph Attr -> B.Builder
-- graph2text = setText . setIndent . sortEdge

-- sortEdge :: Graph Attr -> [Edge Attr]

-- indent :: [Edge Attr] -> [(Indent, Edge Attr)]

-- setText :: [(Indent, Edge Attr)] -> B.Builder




-- json2graph :: Value -> Parser Graph

-- graph2json :: Graph -> Value
