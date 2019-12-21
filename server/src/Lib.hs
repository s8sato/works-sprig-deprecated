{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}



module Lib where

import Prelude hiding (take, drop)

-- Basic for Servant
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-- CORS
import Network.Wai.Middleware.Cors 
import Network.Wai.Middleware.Servant.Options

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
-- import System.IO

import Db
import Database.Persist (Entity(..))
import Control.Monad.IO.Class   (liftIO)

import Data.Double.Conversion.Text (toFixed)

import Database.Persist.Sql (fromSqlKey ,SqlBackend (..), ToBackendKey)

import Query

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

data ElmTask = ElmTask
    { elmTaskId :: Integer
    , elmTaskIsDone :: Bool
    , elmTaskIsStarred :: Bool
    , elmTaskTitle :: Text
    , elmTaskLink :: Text
    , elmTaskStart :: Text
    , elmTaskDeadline :: Text
    , elmTaskWeight :: Double
    , elmTaskSecUntilStart :: Integer
    , elmTaskSecUntilDeadline :: Integer
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmTask)

data ElmModel = ElmModel
    { elmModelUser :: Int
    , elmModelTasks :: [ElmTask]
    , elmModelInputText :: Text
    , elmModelBarLeftEdgeTime :: Text
    , elmModelIndicator :: Int
    }

$(deriveJSON defaultOptions ''ElmModel)

data TextPost = TextPost
    { textPostUser :: Int
    , textPostContent :: Text
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''TextPost)

data DoneTasks = DoneTasks
    { doneTasksUser :: Int
    , doneTasksIds :: [Integer]
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''DoneTasks)

data SwitchStar = SwitchStar
    { switchStarUser :: Int
    , switchStarId :: Integer
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''SwitchStar)

data FocusTask = FocusTask
    { focusTaskUser :: Int
    , focusTaskId :: Integer
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''FocusTask)

type API =  "tasks" :> "all" :> Get '[JSON] ElmModel
    :<|>    "tasks" :> ReqBody '[JSON] TextPost :> Post '[JSON] ElmModel
    :<|>    "tasks" :> "done" :> ReqBody '[JSON] DoneTasks :> Post '[JSON] ElmModel
    :<|>    "tasks" :> "star" :> ReqBody '[JSON] SwitchStar :> Post '[JSON] ()
    :<|>    "tasks" :> "focus" :> ReqBody '[JSON] FocusTask :> Post '[JSON] ElmModel

startApp :: IO ()
startApp = run 8080 app

-- app :: Application
-- app = serve api server
-- app = simpleCors $ serve api server

app :: Application
app = cors (const $ Just policy)
    $ provideOptions api
    $ serve api server
  where
  policy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "content-type" ] }

api :: Proxy API
api = Proxy

server :: Server API
server = undoneElmModel
    :<|> textPostReload
    :<|> doneTasksReload
    :<|> switchStar
    :<|> focusTask

    where
        undoneElmModel :: Handler ElmModel
        undoneElmModel = liftIO $ undoneElmModel' 1
        textPostReload :: TextPost -> Handler ElmModel
        textPostReload = liftIO . textPostReload'
        doneTasksReload :: DoneTasks -> Handler ElmModel
        doneTasksReload = liftIO . doneTasksReload'
        switchStar :: SwitchStar -> Handler ()
        switchStar = liftIO . switchStar'
        focusTask :: FocusTask -> Handler ElmModel
        focusTask = liftIO . focusTask'


stdElmModel :: Int -> [Entity Task] -> Int -> IO ElmModel
stdElmModel user tasks indicator = do
    pool <- pgPool
    now <- zonedTimeToUTC <$> getZonedTime
    let elmTasks = map (toElmTask now) tasks
    zNowStr <- formatTime defaultTimeLocale "%Y/%m/%d %T %a" <$> getZonedTime
    return $ ElmModel user elmTasks "" (pack zNowStr) indicator

undoneElmModel' :: Int -> IO ElmModel
undoneElmModel' user = do
    pool <- pgPool
    tasks <- getUndoneTasks pool user
    stdElmModel user tasks 0

textPostReload' :: TextPost -> IO ElmModel
textPostReload' (TextPost u content) = do
    insTasks $ text2tasks content
    undoneElmModel' u

doneTasksReload' :: DoneTasks -> IO ElmModel
doneTasksReload' (DoneTasks u ids) = do
    pool <- pgPool
    setTasksDone pool ids
    undoneElmModel' u

switchStar' :: SwitchStar -> IO ()
switchStar' (SwitchStar u id) = do
    pool <- pgPool
    setStarSwitched pool id

focusTask' :: FocusTask -> IO ElmModel
focusTask' (FocusTask user task) = do
    pool        <- pgPool
    beforeMe    <- getBeforeMe  pool task
    me          <- getMe        pool task
    afterMe     <- getAfterMe   pool task
    let aroundMe = beforeMe ++ me ++ afterMe
    stdElmModel user aroundMe (Prelude.length beforeMe)



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
    edge2task' as (Task t i False False Nothing Nothing Nothing Nothing Nothing defaultUser)

edge2task' :: [Attr] -> Task -> Task
edge2task' [] task = 
    task
edge2task' (a:as) (Task t i d s ml ms md mw mt u) =
    case a of
        IsDone _ ->
            edge2task' as (Task t i True s ml ms md mw mt u)
        IsStarred _ ->
            edge2task' as (Task t i d True ml ms md mw mt u)
        Link l ->
            edge2task' as (Task t i d s (Just l) ms md mw mt u)
        StartDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case ms of
                    Just (UTCTime od ot) ->
                        edge2task' as (Task t i d s ml (Just (UTCTime nd ot)) md mw mt u)
                    Nothing ->
                        edge2task' as (Task t i d s ml (Just (UTCTime nd 0)) md mw mt u)
        StartTime hh mm ss ->
            let
                nt = secondsToDiffTime $ fromIntegral $ ss + 60 * (mm + 60 * hh)
            in
                case ms of
                    Just (UTCTime od ot) ->
                        edge2task' as (Task t i d s ml (Just (UTCTime od nt)) md mw mt u)
                    Nothing ->
                        let
                            nd = fromGregorian 3000 0 0
                        in
                            edge2task' as (Task t i d s ml (Just (UTCTime nd nt)) md mw mt u)
        DeadlineDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case ms of
                    Just (UTCTime od ot) ->
                        edge2task' as (Task t i d s ml ms (Just (UTCTime nd ot)) mw mt u)
                    Nothing ->
                        edge2task' as (Task t i d s ml ms (Just (UTCTime nd 0)) mw mt u)
        DeadlineTime hh mm ss ->
            let
                nt = secondsToDiffTime $ fromIntegral $ ss + 60 * (mm + 60 * hh)
            in
                case ms of
                    Just (UTCTime od ot) ->
                        edge2task' as (Task t i d s ml ms (Just (UTCTime od nt)) mw mt u)
                    Nothing ->
                        let
                            nd = fromGregorian 3000 0 0
                        in
                            edge2task' as (Task t i d s ml ms (Just (UTCTime nd nt)) mw mt u)
        Weight w ->
            edge2task' as (Task t i d s ml ms md (Just w) mt u)
        Title tt' ->
            case mt of
                Just tt ->
                    edge2task' as (Task t i d s ml ms md mw (Just $ Data.Text.intercalate " " [tt', tt]) u)
                Nothing ->
                    edge2task' as (Task t i d s ml ms md mw (Just tt') u)


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
aAttr = AttrTaskId    <$  char '@'  <*> decimal
    <|> IsDone        <$> char '#'
    <|> IsStarred     <$> char '*'
    <|> Link          <$  char '&'  <*> takeText
    <|> StartDate     <$> decimal   <* char '/' <*> decimal <* char '/' <*> decimal <* char '-'
    <|> StartTime     <$> decimal   <* char ':' <*> decimal <* char ':' <*> decimal <* char '-'
    <|> DeadlineDate  <$  char '-'  <*> decimal <* char '/' <*> decimal <* char '/' <*> decimal
    <|> DeadlineTime  <$  char '-'  <*> decimal <* char ':' <*> decimal <* char ':' <*> decimal
    <|> Weight        <$  char '$'  <*> double
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


-- fileTest :: FilePath -> IO ()
-- fileTest inFile = do
--     withFile inFile ReadMode $ \inHandle ->
--         do  text <- hGetContents inHandle
--             print (text2graph $ pack text)

-- fileTest2 :: FilePath -> IO ()
-- fileTest2 inFile = do
--     withFile inFile ReadMode $ \inHandle ->
--         do  text <- hGetContents inHandle
--             print (text2tasks $ pack text)

-- fileTest3 :: FilePath -> IO ()
-- fileTest3 inFile = do
--     withFile inFile ReadMode $ \inHandle ->
--         do  text <- hGetContents inHandle
--             insTasks $ text2tasks $ pack text



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

idFromEntity :: ToBackendKey SqlBackend record => Entity record -> Integer
idFromEntity = fromIntegral . fromSqlKey . entityKey

toElmTask :: UTCTime -> Entity Task -> ElmTask
-- dots per year
toElmTask now e =
    let
        ei  = idFromEntity e
        Task _ _ d s ml ms md mw mt _ = entityVal e
        ed  = d
        es  = s
        et = case mt of
                Just title ->
                    title
                Nothing ->
                    ""
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
        eus = secUntil now ms
        eud = secUntil now md
    in
        ElmTask ei ed es et el ess edd ew eus eud

toElmTime :: Maybe UTCTime -> Text
toElmTime mt =
    case mt of
        Just t ->
            pack $ formatTime defaultTimeLocale "%0Y/%m/%d %H:%M'%S" t
        Nothing ->
            "----/--/-- --:--'--"

secUntil :: UTCTime -> Maybe UTCTime -> Integer
secUntil now mt =
    case mt of
        Nothing ->
            0
        Just t ->
            floor $ diffUTCTime t now

-- dot = case ms of
--     Just s ->
--         sec2dot dpy $ diffSeconds s now
--     Nothing ->
--         0
-- sha = case mw of
--     Just w ->
--         (+) 1 $ sec2dot dpy $ weight2sec w 
--     Nothing ->
--         0
-- exc = case md of
--     Just d ->
--         sec2dot dpy $ diffSeconds d now 
--     Nothing ->
--         -1

-- diffSeconds :: UTCTime -> UTCTime -> Integer
-- diffSeconds t1 t2 = floor $ diffUTCTime t1 t2

-- sec2dot :: Int -> Integer -> Int
-- sec2dot dpy sec =
--     (dpy * fromIntegral(sec)) `div` (60 * 60 * 24 * 365)

-- weight2sec :: Double -> Integer
-- weight2sec w = floor (60 * 60 * w)


--


