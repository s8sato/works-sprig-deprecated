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

import Db
import Database.Persist (Entity(..))
import Control.Monad.IO.Class   (liftIO)

yeaPerY = 1
quaPerY = 4
monPerY = 12
weePerY = 52
dayPerY = 365
houPerY = 8760
minPerY = 525600
secPerY = 31536000

defaultTaskLink = ""

data ElmBar = ElmBar
    { elmBarDot :: Int
    , elmBarSha :: Int
    , elmBarExc :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmBar)

data ElmTask = ElmTask
    { elmTaskIsDone :: Bool
    , elmTaskIsStarred :: Bool
    , elmTaskTitle :: String
    , elmTaskLink :: String
    , elmTaskStart :: String
    , elmTaskDeadline :: String
    , elmTaskWeight :: Double
    , elmTaskBar :: ElmBar
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmTask)

-- data ElmModel = Model
--     { elmModelDpy :: Int
--     , elmModelTasks :: [Task]
--     } deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''ElmModel)

type API = "tasks" :> "all" :> Get '[JSON] [ElmTask]

startApp :: IO ()
startApp = run 8080 app

app :: Application
-- app = serve api server
app = simpleCors $ serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = liftIO ggeett

ggeett = do
    pool <- pgPool
    entities <- getUndoneTasksByUser pool 1
    now <- zonedTimeToUTC <$> getZonedTime
    let elmTasks = map (toElmTask dayPerY now) (map entity2a entities)
    return elmTasks


entity2a :: Entity a -> a
entity2a (Entity k v) = v

-- 

type Node  = Int
data Attr  =  AttrTaskId       { attrTaskId       :: Int    }
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
-- derivePersistField "Attr"
data Edge  = Edge          { terminal     :: Node, initial       :: Node, attrs          :: [Attr] }
    deriving (Eq, Show)
-- derivePersistField "Edge"
data Graph = Graph         { edges        :: [Edge] }
            deriving (Eq, Show)
-- derivePersistField "Graph"

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
aAttr =       AttrTaskId        <$  string "@"    <*> decimal
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
-- attr2json (AttrTaskId i) =
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

toElmTask :: Int -> UTCTime -> Task -> ElmTask
-- dots per year
toElmTask dpy now (Task _ _ d s ml ms md mw tt _) =
    let
        ed  = d
        es  = s
        ett = tt
        el  = case ml of
                Just link ->
                    link
                Nothing ->
                    defaultTaskLink
        ess = toElmTime ms
        edd = toElmTime md
        ew  = case mw of
                Just weight ->
                    weight
                Nothing ->
                    0
        eb  = toElmBar dpy now ms mw md
    in
        ElmTask ed es ett el ess edd ew eb 

toElmTime :: Maybe UTCTime -> String
toElmTime mt =
    case mt of
        Just t ->
            formatTime defaultTimeLocale "%Y/%m/%d %H:%M'%S" t
        Nothing ->
            "----/--/-- --:--'--"

toElmBar :: Int -> UTCTime -> Maybe UTCTime -> Maybe Double -> Maybe UTCTime -> ElmBar
toElmBar dpy now ms mw md =
    let
        dot = case ms of
            Just s ->
                sec2dot dpy $ diffSeconds s now
            Nothing ->
                0
        sha = case mw of
            Just w ->
                (+) 1 $ sec2dot dpy $ weight2sec w 
            Nothing ->
                0
        exc = case md of
            Just d ->
                sec2dot dpy $ diffSeconds d now 
            Nothing ->
                -1
    in
        ElmBar dot sha exc

diffSeconds :: UTCTime -> UTCTime -> Integer
diffSeconds t1 t2 = floor $ diffUTCTime t1 t2

sec2dot :: Int -> Integer -> Int
sec2dot dpy sec =
    (dpy * fromIntegral(sec)) `div` (60 * 60 * 24 * 365)

weight2sec :: Double -> Integer
weight2sec w = floor (60 * 60 * w)


--


