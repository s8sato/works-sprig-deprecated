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

-- for fileTest
import System.IO

import Db
import Database.Persist (Entity(..))
import Control.Monad.IO.Class   (liftIO)

import Data.Double.Conversion.Text (toFixed)

yeaPerY = 1
quaPerY = 4
monPerY = 12
weePerY = 52
dayPerY = 365
houPerY = 8760
minPerY = 525600
secPerY = 31536000

defaultTaskLink = pack ""
defaultUser = 1

data ElmBar = ElmBar
    { elmBarDot :: Int
    , elmBarSha :: Int
    , elmBarExc :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmBar)

data ElmTask = ElmTask
    { elmTaskIsDone :: Bool
    , elmTaskIsStarred :: Bool
    , elmTaskTitle :: Text
    , elmTaskLink :: Text
    , elmTaskStart :: Text
    , elmTaskDeadline :: Text
    , elmTaskWeight :: Double
    , elmTaskBar :: ElmBar
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmTask)

-- data ElmModel = Model
--     { elmModelDpy :: Int
--     , elmModelTasks :: [Task]
--     } deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''ElmModel)

type API =  "tasks" :> "all" :> Get '[JSON] [ElmTask]
    :<|>    "tasks" :> ReqBody '[JSON] TextPost :> Post '[JSON] [ElmTask]

data TextPost = TextPost
    { textPostUser :: Int
    , textPostContent :: Text
    , textPostDpy :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''TextPost)
-- instance FromJSON TextPost
-- instance ToJSON TextPost

textPostReload' :: TextPost -> IO [ElmTask]
textPostReload' (TextPost u c d) = do
    insTasks $ text2tasks c
    getUndoneElmTasks u d

startApp :: IO ()
startApp = run 8080 app

app :: Application
-- app = serve api server
app = simpleCors $ serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = (liftIO $ getUndoneElmTasks 1 dayPerY) 
    :<|> textPostReload
    where
        textPostReload :: TextPost -> Handler [ElmTask]
        textPostReload tp = liftIO $ textPostReload' tp

getUndoneElmTasks :: Int -> Int -> IO [ElmTask]
getUndoneElmTasks user dpy = do
    pool <- pgPool
    entities <- getUndoneTasks pool user
    now <- zonedTimeToUTC <$> getZonedTime
    return $ map (toElmTask dpy now) (map entity2v entities)


entity2v :: Entity a -> a
entity2v (Entity _ v) = v

-- 

type Graph = [Edge]
type Edge = ((Node, Node), [Attr])
type Node  = Int
data Attr  =  AttrTaskId       { attrTaskId       :: Int    }
    | IsDone       { isDone       :: Char }
    | IsStarred    { isStarred    :: Char }
    | Link         { link         :: Text }
    | StartDate    { startYear    :: Int, startMonth     :: Int,  startDay       :: Int    }
    | StartTime    { startHour    :: Int, startMinute    :: Int,  startSecond    :: Int    }
    | DeadlineDate { deadlineYear :: Int, deadlineMonth  :: Int,  deadlineDay    :: Int    }
    | DeadlineTime { deadlineHour :: Int, deadlineMinute :: Int,  deadlineSecond :: Int    }
    | Weight       { weight       :: Double }
    | Title        { title        :: Text }
    deriving (Eq, Show)

text2tasks :: Text -> [Task]
text2tasks = graph2tasks . text2graph

graph2tasks :: Graph -> [Task]
graph2tasks = map edge2task

edge2task :: Edge -> Task
edge2task ((t,i),as) =
    edge2task' as (Task t i False False Nothing Nothing Nothing Nothing "" defaultUser)

edge2task' :: [Attr] -> Task -> Task
edge2task' [] task = 
    task
edge2task' (a:as) (Task t i d s ml ms md mw tt u) =
    case a of
        IsDone '>' ->
            edge2task' as (Task t i True s ml ms md mw tt u)
        IsDone _ ->
            edge2task' as (Task t i False s ml ms md mw tt u)
        IsStarred '>' ->
            edge2task' as (Task t i d True ml ms md mw tt u)
        IsStarred _ ->
            edge2task' as (Task t i d False ml ms md mw tt u)
        Link l ->
            edge2task' as (Task t i d s (Just l) ms md mw tt u)
        StartDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case ms of
                    Just (UTCTime od ot) ->
                        edge2task' as (Task t i d s ml (Just (UTCTime nd ot)) md mw tt u)
                    Nothing ->
                        edge2task' as (Task t i d s ml (Just (UTCTime nd 0)) md mw tt u)
        StartTime hh mm ss ->
            let
                nt = secondsToDiffTime $ fromIntegral $ ss + 60 * (mm + 60 * hh)
            in
                case ms of
                    Just (UTCTime od ot) ->
                        edge2task' as (Task t i d s ml (Just (UTCTime od nt)) md mw tt u)
                    Nothing ->
                        let
                            nd = fromGregorian 0 0 0
                        in
                            edge2task' as (Task t i d s ml (Just (UTCTime nd nt)) md mw tt u)
        DeadlineDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case ms of
                    Just (UTCTime od ot) ->
                        edge2task' as (Task t i d s ml ms (Just (UTCTime nd ot)) mw tt u)
                    Nothing ->
                        edge2task' as (Task t i d s ml ms (Just (UTCTime nd 0)) mw tt u)
        DeadlineTime hh mm ss ->
            let
                nt = secondsToDiffTime $ fromIntegral $ ss + 60 * (mm + 60 * hh)
            in
                case ms of
                    Just (UTCTime od ot) ->
                        edge2task' as (Task t i d s ml ms (Just (UTCTime od nt)) mw tt u)
                    Nothing ->
                        let
                            nd = fromGregorian 0 0 0
                        in
                            edge2task' as (Task t i d s ml ms (Just (UTCTime nd nt)) mw tt u)
        Weight w ->
            edge2task' as (Task t i d s ml ms md (Just w) tt u)
        Title tt' ->
            edge2task' as (Task t i d s ml ms md mw (Data.Text.concat[tt', tt]) u)

text2graph :: Text -> Graph
text2graph = spanLink . assemble . markUp . chopLines

type Indent = Int
indent :: Text
indent = "    "

chopLines :: Text -> [(Indent, [Text])]
chopLines t =  map indentAndWords $ filter (/= "") $ splitOn "\n" t

indentAndWords :: Text -> (Indent, [Text])
indentAndWords = indentAndWords' 0

indentAndWords' :: Int -> Text -> (Indent, [Text])
indentAndWords' c t
    | take l t == indent    = indentAndWords' (c + 1) (drop l t)
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

aAttr :: Parser Attr
aAttr = AttrTaskId        <$  string "@"    <*> decimal
        -- TODO if anyChar machies empty
    <|> IsDone        <$  string "</"    <*> anyChar
    <|> IsStarred     <$  string "<*"    <*> anyChar
    <|> Link          <$  string "&"    <*> takeText
    <|> StartDate     <$  string ""     <*> decimal <* char '/' <*> decimal <* char '/' <*> decimal <* char '-'
    <|> StartTime     <$  string ""     <*> decimal <* char ':' <*> decimal <* char ':' <*> decimal <* char '-'
    <|> DeadlineDate  <$  string "-"    <*> decimal <* char '/' <*> decimal <* char '/' <*> decimal
    <|> DeadlineTime  <$  string "-"    <*> decimal <* char ':' <*> decimal <* char ':' <*> decimal
    <|> Weight        <$  string "$"    <*> double
    <|> Title         <$> takeText

assemble  :: [((Node, Node), [Text])] -> Graph
assemble xs = map (assemble' []) xs

assemble' :: [Attr] -> ((Node, Node), [Text]) -> Edge
assemble' mem ((t,i), []) = ((t,i), mem)
assemble' mem ((t,i), a:as)  = 
    case parseOnly aAttr a of
        Left _ -> ((t,i), mem)
        Right r -> assemble' (r:mem) ((t,i), as)

spanLink :: Graph -> Graph
spanLink g = g


fileTest :: FilePath -> IO ()
fileTest inFile = do
    withFile inFile ReadMode $ \inHandle ->
        do  text <- hGetContents inHandle
            print (text2graph $ pack text)

fileTest2 :: FilePath -> IO ()
fileTest2 inFile = do
    withFile inFile ReadMode $ \inHandle ->
        do  text <- hGetContents inHandle
            print (text2tasks $ pack text)

fileTest3 :: FilePath -> IO ()
fileTest3 inFile = do
    withFile inFile ReadMode $ \inHandle ->
        do  text <- hGetContents inHandle
            insTasks $ text2tasks $ pack text



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

toElmTime :: Maybe UTCTime -> Text
toElmTime mt =
    case mt of
        Just t ->
            pack $ formatTime defaultTimeLocale "%Y/%m/%d %H:%M'%S" t
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


