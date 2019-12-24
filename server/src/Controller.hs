{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}



module Controller where

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
-- for jsonTo_
import Data.Aeson.Types hiding (Parser)
import Control.Applicative
-- for jsonToClient
import qualified Data.HashMap.Strict as M

-- parse
import Data.Attoparsec.Text hiding (take)
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

import Entity
import Database.Persist (Entity(..))
import Control.Monad.IO.Class   (liftIO)

import Data.Double.Conversion.Text (toFixed)

import Database.Persist.Sql (fromSqlKey ,SqlBackend (..), ToBackendKey, toSqlKey)

import Query

import Data.Tuple.Extra (both)

import System.Posix.Types (EpochTime)
import Foreign.C.Types (CTime (..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
-- import Data.Time.Clock.Internal.NominalDiffTime (nominalDiffTimeToSeconds)
import qualified Database.Esqueleto as Q (Value (..)) 



-- type alias



-- yeaPerY = 1
-- quaPerY = 4
-- monPerY = 12
-- weePerY = 52
-- dayPerY = 365
-- houPerY = 8760
-- minPerY = 525600
-- secPerY = 31536000

defaultUser = toSqlKey 1 :: UserId



-- DATA DECLARATION



data ElmUser = ElmUser
    { elmUserId :: Int
    , elmUserName :: Text
    , elmUserAdmin :: Bool
    -- , elmUserDefaultDpy :: Maybe Int
    -- , elmUserLookUp :: Maybe Int
    -- , elmUserLookDown :: Maybe Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmUser)

data ElmTask = ElmTask
    { elmTaskId :: Integer
    , elmTaskIsDone :: Bool
    , elmTaskIsStarred :: Bool
    , elmTaskTitle :: Maybe Text
    , elmTaskLink :: Maybe Text
    , elmTaskStart :: Maybe Integer
    , elmTaskDeadline :: Maybe Integer
    , elmTaskWeight :: Maybe Double
    , elmTaskUser :: Text
    -- , elmTaskSecUntilStart :: Maybe Integer
    -- , elmTaskSecUntilDeadline :: Maybe Integer
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmTask)

data ElmSubModel = ElmSubModel
    { elmSubModelUser :: ElmUser
    , elmSubModelTasks :: [ElmTask]
    , elmSubModelInputText :: Maybe Text
    , elmSubModelDpy :: Maybe Int
    , elmSubModelMessage :: Maybe Text
    }

$(deriveJSON defaultOptions ''ElmSubModel)

data Initial = Initial
    { initialUser :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Initial)

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
    { switchStarId :: Integer
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''SwitchStar)

data FocusTask = FocusTask
    { focusTaskId :: Integer
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''FocusTask)



-- API DEFINITION



type API =  "dev"   :> "sModel" :> Capture "userId" Int         :> Get  '[JSON] ElmSubModel
    :<|>    "tasks" :> "init"   :> ReqBody '[JSON] Initial      :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "post"   :> ReqBody '[JSON] TextPost     :> Post '[JSON] [ElmTask]
    :<|>    "tasks" :> "done"   :> ReqBody '[JSON] DoneTasks    :> Post '[JSON] [ElmTask]
    :<|>    "tasks" :> "star"   :> ReqBody '[JSON] SwitchStar   :> Post '[JSON] ()
    :<|>    "tasks" :> "focus"  :> ReqBody '[JSON] FocusTask    :> Post '[JSON] [ElmTask]

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
server = devSubModel
    :<|> initialize
    :<|> textPostReload
    :<|> doneTasksReload
    :<|> switchStar
    :<|> focusTask
    where
        devSubModel :: Int -> Handler ElmSubModel
        devSubModel = liftIO . devSubModel'
        initialize :: Initial -> Handler ElmSubModel
        initialize = liftIO . initialize'
        textPostReload :: TextPost -> Handler [ElmTask]
        textPostReload = liftIO . textPostReload'
        doneTasksReload :: DoneTasks -> Handler [ElmTask]
        doneTasksReload = liftIO . doneTasksReload'
        switchStar :: SwitchStar -> Handler ()
        switchStar = liftIO . switchStar'
        focusTask :: FocusTask -> Handler [ElmTask]
        focusTask = liftIO . focusTask'

devSubModel' :: Int -> IO ElmSubModel
devSubModel' uid = do
    pool <- pgPool
    eUs <- getUserById pool uid
    taskAssigns <- getUndoneTaskAssigns pool uid
    let elmUser = toElmUser . Prelude.head $ eUs
    let elmTasks = map toElmTask taskAssigns 
    let mDpy =  userDefaultDpy . entityVal . Prelude.head $ eUs
    return $ ElmSubModel elmUser elmTasks Nothing mDpy Nothing

initialize' :: Initial -> IO ElmSubModel
initialize' (Initial uid) = do
    pool <- pgPool
    eUs <- getUserById pool uid
    taskAssigns <- getUndoneTaskAssigns pool uid
    let elmUser = toElmUser . Prelude.head $ eUs
    let elmTasks = map toElmTask taskAssigns 
    let mDpy =  userDefaultDpy . entityVal . Prelude.head $ eUs
    return $ ElmSubModel elmUser elmTasks Nothing mDpy Nothing


getUndoneElmTasks :: Int -> IO [ElmTask]
getUndoneElmTasks user = do
    pool <- pgPool
    taskAssigns <- getUndoneTaskAssigns pool user
    return $ map toElmTask taskAssigns 

textPostReload' :: TextPost -> IO [ElmTask]
textPostReload' (TextPost u text) = do
    mk <- maybeMaxTaskIdKey
    let max = case mk of
            Nothing -> 0
            Just k  -> idFromKey k
    let shift = max + 2  -- TODO 
    insTasks . (shiftTaskNodes shift) . tasksFromText $ text
    getUndoneElmTasks u

doneTasksReload' :: DoneTasks -> IO [ElmTask]
doneTasksReload' (DoneTasks u ids) = do
    pool <- pgPool
    setTasksDone pool ids
    getUndoneElmTasks u

switchStar' :: SwitchStar -> IO ()
switchStar' (SwitchStar id) = do
    pool <- pgPool
    setStarSwitched pool id

focusTask' :: FocusTask -> IO [ElmTask]
focusTask' (FocusTask id) = do
    pool        <- pgPool
    beforeMe    <- getBeforeMe  pool id
    me          <- getMe        pool id
    afterMe     <- getAfterMe   pool id
    return . map toElmTask $ Prelude.concat [beforeMe, me, afterMe]



-- INTERNAL OPERATIONS



type Graph = [Edge]
type Edge = ((Node, Node), [Attr])
type Node  = Int
data Attr  = AttrTaskId { attrTaskId :: Integer }
    | IsDone       { isDone      :: Char }
    | IsStarred    { isStarred   :: Char }
    | Link         { link        :: Text }
    | StartDate    { startYea    :: Int, startMon    :: Int, startDay    :: Int }
    | StartTime    { startHou    :: Int, startMin    :: Int, startSec    :: Int }
    | DeadlineDate { deadlineYea :: Int, deadlineMon :: Int, deadlineDay :: Int }
    | DeadlineTime { deadlineHou :: Int, deadlineMin :: Int, deadlineSec :: Int }
    | Weight       { weight      :: Double }
    | HeadLink     { headLink    :: Text }
    | TailLink     { tailLink    :: Text }
    | Title        { title       :: Text }
    deriving (Eq, Show)

tasksFromText :: Text -> [Task]
tasksFromText = tasksFromGraph . graphFromText

tasksFromGraph :: Graph -> [Task]
tasksFromGraph = map taskFromEdge

taskFromEdge :: Edge -> Task
taskFromEdge ((t,i),as) =
    taskFromEdge' as (Task t i False False Nothing Nothing Nothing Nothing Nothing defaultUser)

taskFromEdge' :: [Attr] -> Task -> Task
taskFromEdge' [] task = 
    task
taskFromEdge' (a:as) (Task t i d s ml ms md mw mt u) =
    case a of
        AttrTaskId 0 ->
            taskFromEdge' as (Task t i d s ml ms md mw mt u)
        AttrTaskId n ->  -- TODO
            taskFromEdge' as (Task t i d s ml ms md mw mt u)
        IsDone _ ->
            taskFromEdge' as (Task t i True s ml ms md mw mt u)
        IsStarred _ ->
            taskFromEdge' as (Task t i d True ml ms md mw mt u)
        Link l ->
            taskFromEdge' as (Task t i d s (Just l) ms md mw mt u)
        StartDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case ms of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as (Task t i d s ml (Just (UTCTime nd ot)) md mw mt u)
                    Nothing ->
                        taskFromEdge' as (Task t i d s ml (Just (UTCTime nd 0)) md mw mt u)
        StartTime hh mm ss ->
            let
                nt = secondsToDiffTime $ fromIntegral $ ss + 60 * (mm + 60 * hh)
            in
                case ms of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as (Task t i d s ml (Just (UTCTime od nt)) md mw mt u)
                    Nothing ->
                        let
                            nd = fromGregorian 3000 0 0
                        in
                            taskFromEdge' as (Task t i d s ml (Just (UTCTime nd nt)) md mw mt u)
        DeadlineDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case ms of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as (Task t i d s ml ms (Just (UTCTime nd ot)) mw mt u)
                    Nothing ->
                        taskFromEdge' as (Task t i d s ml ms (Just (UTCTime nd 0)) mw mt u)
        DeadlineTime hh mm ss ->
            let
                nt = secondsToDiffTime $ fromIntegral $ ss + 60 * (mm + 60 * hh)
            in
                case ms of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as (Task t i d s ml ms (Just (UTCTime od nt)) mw mt u)
                    Nothing ->
                        let
                            nd = fromGregorian 3000 0 0
                        in
                            taskFromEdge' as (Task t i d s ml ms (Just (UTCTime nd nt)) mw mt u)
        Weight w ->
            taskFromEdge' as (Task t i d s ml ms md (Just w) mt u)
        Title tt' ->
            case mt of
                Just tt ->
                    taskFromEdge' as (Task t i d s ml ms md mw (Just $ Data.Text.intercalate " " [tt', tt]) u)
                Nothing ->
                    taskFromEdge' as (Task t i d s ml ms md mw (Just tt') u)
        _ -> 
            taskFromEdge' as (Task t i d s ml ms md mw mt u)

graphFromText :: Text -> Graph
graphFromText = spanLink . assemble . markUp . chopLines

type Indent = Int
indent :: Text
indent = "    "

chopLines :: Text -> [(Indent, [Text])]
chopLines = map indentAndWords . filter (/= "") . splitOn "\n"

indentAndWords :: Text -> (Indent, [Text])
indentAndWords = indentAndWords' 0

indentAndWords' :: Int -> Text -> (Indent, [Text])
indentAndWords' c t
    | take l t == indent    = indentAndWords' (c + 1) (drop l t)
    | otherwise             = (c, filter (/= "") $ splitOn " " t)
    where l = Data.Text.length indent

markUp :: [(Indent, [Text])] -> [((Node, Node), [Text])]
markUp xs =
    let
        pairs = shiftConcat . (map markUp') . groupsFrom $ map fst xs
    in
        zip pairs (map snd xs)

groupsFrom :: [Indent] -> [[Indent]]
groupsFrom is = reverse . map reverse $ groupsFrom' is [] []

groupsFrom' :: [Indent] -> [Indent] -> [[Indent]] -> [[Indent]]
groupsFrom' [] memory store  = memory : store
groupsFrom' (0:is) [] s      = groupsFrom' is [0] s
groupsFrom' (0:is) m s       = groupsFrom' is [0] (m:s)
groupsFrom' (n:is) m s       = groupsFrom' is (n:m) s

shiftConcat :: [[(Node, Node)]] -> [(Node, Node)]
shiftConcat pss = Prelude.concat . reverse $ shiftConcat' pss 0 []

shiftConcat' :: [[(Node, Node)]] -> Int -> [[(Node, Node)]] -> [[(Node, Node)]]
shiftConcat' [] _ store = store
shiftConcat' ([]:r) shift s = shiftConcat' r shift s
shiftConcat' (pairs:r) shift s = 
    shiftConcat' r (1 + shift + Prelude.length pairs) ((shiftNodes shift pairs) : s)

shiftNodes :: Int -> [(Node, Node)] -> [(Node, Node)]
shiftNodes shift = map . both $ (+) shift

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
aAttr = AttrTaskId    <$  char '#'  <*> decimal
    <|> IsDone        <$> char '%'
    <|> IsStarred     <$> char '*'
    <|> Link          <$  char '&'  <*> takeText
    <|> StartDate     <$> decimal   <*  char '/' <*> decimal <* char '/' <*> decimal <* char '-'
    <|> StartTime     <$> decimal   <*  char ':' <*> decimal <* char ':' <*> decimal <* char '-'
    <|> DeadlineDate  <$  char '-'  <*> decimal <* char '/' <*> decimal <* char '/' <*> decimal
    <|> DeadlineTime  <$  char '-'  <*> decimal <* char ':' <*> decimal <* char ':' <*> decimal
    <|> Weight        <$  char '$'  <*> double
    <|> HeadLink      <$  char ']'  <*> takeText
    <|> TailLink      <$  char '['  <*> takeText
    <|> Title         <$> takeText

assemble  :: [((Node, Node), [Text])] -> Graph
assemble = map $ assemble' []

assemble' :: [Attr] -> ((Node, Node), [Text]) -> Edge
assemble' mem ((t,i), []) = ((t,i), mem)
assemble' mem ((t,i), a:as)  = 
    case parseOnly aAttr a of
        Left _ -> ((t,i), mem)
        Right r -> assemble' (r:mem) ((t,i), as)

spanLink :: Graph -> Graph
-- TODO
spanLink g = spanLink' g g g

spanLink' :: [Edge] -> [Edge] -> Graph -> Graph
spanLink' [] _ g                    = g
spanLink' (t:ts) [] g               = spanLink' ts g g
spanLink' ((p,at):ts) ((q,ah):hs) g = 
    if keyMatch at ah ah then
        spanLink' ((p,at):ts) hs (spanLink'' p q g)
    else
        spanLink' ((p,at):ts) hs g

keyMatch :: [Attr] -> [Attr] -> [Attr] -> Bool
keyMatch [] _ _ = False
keyMatch (a:as) [] ah = keyMatch as ah ah
keyMatch (a:as) (b:bs) ah = case a of
    TailLink t ->
        case b of
            HeadLink t ->
                True
            _ ->
                keyMatch (a:as) bs ah
    _ ->
        keyMatch as (b:bs) ah

spanLink'' :: (Node, Node) -> (Node, Node) -> Graph -> Graph
spanLink'' (_,t) (i,_) g = ((t,i), [AttrTaskId 0, IsDone '%', Title "LINKER"]) : g

    --     headLs = map (\(, ) g
    --         map
    -- in
    --     spanLink' headLs tailLs

-- fileTest :: FilePath -> IO ()
-- fileTest inFile = do
--     withFile inFile ReadMode $ \inHandle ->
--         do  text <- hGetContents inHandle
--             print (graphFromText $ pack text)

-- fileTest2 :: FilePath -> IO ()
-- fileTest2 inFile = do
--     withFile inFile ReadMode $ \inHandle ->
--         do  text <- hGetContents inHandle
--             print (tasksFromText $ pack text)

-- fileTest3 :: FilePath -> IO ()
-- fileTest3 inFile = do
--     withFile inFile ReadMode $ \inHandle ->
--         do  text <- hGetContents inHandle
--             insTasks $ tasksFromText $ pack text



-- textFromGraph :: Graph [Attr] -> B.Builder
-- textFromGraph = setText . setIndent . sortEdge

-- sortEdge :: Graph [Attr] -> [Edge [Attr]]

-- indent :: [Edge [Attr]] -> [(Indent, Edge [Attr])]

-- setText :: [(Indent, Edge [Attr])] -> B.Builder



-- graphFromJson :: Value -> Parser Graph

-- jsonFromGraph :: Graph -> Value
-- jsonFromGraph (Graph es) =
--     object  [ "edges"   .= object [ jsonFromEdge es ] 
--             ]

-- jsonFromEdge :: Edge -> Value
-- jsonFromEdge (Edge t i as) =
--     object  [ "terminal"    .= Number (fromIntegral t)
--             , "initial"     .= Number (fromInteger i)
--             , "attrs"       .= object [ jsonFromAttr as ]
--             ]

-- jsonFromAttr :: Attr -> Value
-- jsonFromAttr (AttrTaskId i) =
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

idFromEntity :: (Integral a, ToBackendKey SqlBackend record) => Entity record -> a
idFromEntity = idFromKey . entityKey

idFromKey :: (Integral a, ToBackendKey SqlBackend record) => Key record -> a
idFromKey = fromIntegral . fromSqlKey

toElmTask :: (Entity Task, Q.Value Text) -> ElmTask
toElmTask (e, u) =
    let
        i  = idFromEntity e
        Task _ _ d s ml ms md mw mt _ = entityVal e
        ems = toElmTime ms
        emd = toElmTime md
        eu  = Q.unValue u
    in
        ElmTask i d s mt ml ems emd mw eu

toElmUser :: Entity User -> ElmUser
toElmUser e =
    let
        i  = idFromEntity e
        User n a _ _ _ _ = entityVal e
    in
        ElmUser i n a

toElmTime :: Maybe UTCTime -> Maybe Integer
toElmTime Nothing = 
    Nothing
toElmTime (Just t) =
    Just (floor $ utcTimeToPOSIXSeconds t)

-- secUntil :: UTCTime -> Maybe UTCTime -> Maybe Integer
-- secUntil now Nothing =
--     Nothing
-- secUntil now (Just t) =
--     Just . floor $ diffUTCTime t now

-- dot = case ms of
--     Just s ->
--         dotsFromSec dpy $ diffSeconds s now
--     Nothing ->
--         0
-- sha = case mw of
--     Just w ->
--         (+) 1 $ dotsFromSec dpy $ SecFromWeight w 
--     Nothing ->
--         0
-- exc = case md of
--     Just d ->
--         dotsFromSec dpy $ diffSeconds d now 
--     Nothing ->
--         -1

-- diffSeconds :: UTCTime -> UTCTime -> Integer
-- diffSeconds t1 t = floor $ diffUTCTime t1 t

-- dotsFromSec :: Int -> Integer -> Int
-- dotsFromSec dpy sec =
--     (dpy * fromIntegral(sec)) `div` (60 * 60 * 24 * 365)

-- SecFromWeight :: Double -> Integer
-- SecFromWeight w = floor (60 * 60 * w)


shiftTaskNodes :: Integer -> [Task] -> [Task]
shiftTaskNodes sh ts =
    map (
        \(Task t i d s l ss dd w tt u) -> 
            let 
                (t',i') = both ((+) $ fromIntegral sh) (t,i)
            in
                Task t' i' d s l ss dd w tt u
        ) ts

uToE :: UTCTime -> EpochTime
uToE = CTime . truncate . utcTimeToPOSIXSeconds

