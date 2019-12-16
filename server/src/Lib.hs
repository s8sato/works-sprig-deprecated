{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

-- {-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

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
import Data.Aeson.Types hiding (Parser)
import Control.Applicative
-- for jsonToClient
import qualified Data.HashMap.Strict as M

-- parse
import Control.Applicative
import Data.Attoparsec.Text hiding (take)
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
import System.IO

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

data Graph = Graph         { edges        :: [Edge] }
            deriving (Eq, Show)
derivePersistField "Graph"
data Edge  = Edge          { terminal     :: Node, initial       :: Node, attrs          :: [Attr] }
            deriving (Eq, Show)
derivePersistField "Edge"
type Node  = Int
data Attr  =  TaskId       { taskId       :: Int    }
            | IsDone       { isDone       :: Text }
            | IsStarred    { isStarred    :: Text }
            | Link         { link         :: String }
            | StartDate    { startYear    :: Int, startMonth     :: Int,  startDay       :: Int    }
            | StartTime    { startHour    :: Int, startMinute    :: Int,  startSecond    :: Int    }
            | DeadlineDate { deadlineYear :: Int, deadlineMonth  :: Int,  deadlineDay    :: Int    }
            | DeadlineTime { deadlineHour :: Int, deadlineMinute :: Int,  deadlineSecond :: Int    }
            | Weight       { weight       :: Int    }
            | Title        { title        :: String }
            deriving (Eq, Show)
derivePersistField "Attr"

text2graph :: Text -> Graph
text2graph = spanLink . assemble . markUp . chopLines

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
markUp' is = markUp'' l l is []
    where l = Prelude.length is - 1 

markUp'' :: Int -> Int -> [Indent] -> [(Node, Node)] -> [(Node, Node)]
markUp'' sbj obj is mem
    | sbj == -1             = mem
    | obj == -1             = markUp'' (sbj - 1) (sbj - 1) is ((-1,sbj):mem)
    | is !! sbj > is !! obj = markUp'' (sbj - 1) (sbj - 1) is ((obj,sbj):mem)
    | otherwise             = markUp'' sbj (obj - 1) is mem

aChar :: Parser Char
aChar =     (const ',') <$> (string "\\,")
        <|> (const '\n') <$> (string "\\n")
        <|> (const '(') <$> (string "\\(")
        <|> (const ')') <$> (string "\\)")
        <|> satisfy (notInClass ",\n()")

aString :: Parser String
aString = many aChar

aAttr :: Parser Attr
aAttr =       TaskId        <$  string "@"    <*> decimal
          <|> IsDone        <$> string "</>"
          <|> IsStarred     <$> string "<*>"
          <|> Link          <$  string "&"    <*> aString
          <|> StartDate     <$  string ""     <*> decimal <* char '/' <*> decimal <* char '/' <*> decimal <* char '-'
          <|> StartTime     <$  string ""     <*> decimal <* char ':' <*> decimal <* char ':' <*> decimal <* char '-'
          <|> DeadlineDate  <$  string "-"    <*> decimal <* char '/' <*> decimal <* char '/' <*> decimal
          <|> DeadlineTime  <$  string "-"    <*> decimal <* char ':' <*> decimal <* char ':' <*> decimal
          <|> Weight        <$  string "$"    <*> decimal
          <|> Title         <$> aString

assemble  :: [((Node, Node), [Text])] -> Graph
assemble xs = Graph (map (assemble' []) xs)

assemble' :: [Attr] -> ((Node, Node), [Text]) -> Edge
assemble' mem ((t,i), []) = Edge t i mem
assemble' mem ((t,i), a:as)  = 
    case parseOnly aAttr a of
        Left _ -> Edge t i mem
        Right r -> assemble' (r:mem) ((t,i), as)

spanLink :: Graph -> Graph
spanLink g = g


fileTest :: FilePath -> IO ()
fileTest inFile = do
    withFile inFile ReadMode $ \inHandle ->
        do  text <- hGetContents inHandle
            print (text2graph $ pack text)

-- graph2text :: Graph [Attr] -> B.Builder
-- graph2text = setText . setIndent . sortEdge

-- sortEdge :: Graph [Attr] -> [Edge [Attr]]

-- indent :: [Edge [Attr]] -> [(Indent, Edge [Attr])]

-- setText :: [(Indent, Edge [Attr])] -> B.Builder



-- json2graph :: Value -> Parser Graph

-- graph2json :: Graph -> Value
-- graph2json (Graph es) =
--     object  [ "edges"   .= object [ edge2json es ] 
--             ]

-- edge2json :: Edge -> Value
-- edge2json (Edge t i as) =
--     object  [ "terminal"    .= Number (fromIntegral t)
--             , "initial"     .= Number (fromInteger i)
--             , "attrs"       .= object [ attr2json as ]
--             ]

-- attr2json :: Attr -> Value
-- attr2json (TaskId i) =
--     object  [ "taskId"      .= Number (fromInteger i)
--             ]




                --         <
                --         <
                -- IsStarred     <
                -- Link          <
                -- StartDate     <
                -- StartTime     <
                -- DeadlineDate  <
                -- DeadlineTime  <
                -- Weight        <
                -- Title         <


-- clientToJSON (Company i n p d) =
--     object  [ "type"    .= String "company"
--             , "id"      .= Number (fromInteger i)
--             , "name"    .= String (pack n)
--             , "person"  .= personToJSON p
--             , "duty"    .= String (pack d)
--             ]


--

