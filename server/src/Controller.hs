{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}



module Controller where

import Prelude hiding (take, drop)

-- Basic for Servant
import Data.Aeson hiding (pair, pairs)
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-- CORS
import Network.Wai.Middleware.Cors 
import Network.Wai.Middleware.Servant.Options

-- json
import Data.Text hiding (map, filter, reverse, zip)
-- for jsonTo_
import Data.Aeson.Types hiding (Parser, pair, pairs)
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

import Database.Persist.Sql (fromSqlKey ,SqlBackend (..), ToBackendKey, toSqlKey, ConnectionPool)

import Query

import Data.Tuple.Extra (both)

import System.Posix.Types (EpochTime)
import Foreign.C.Types (CTime (..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
-- import Data.Time.Clock.Internal.NominalDiffTime (nominalDiffTimeToSeconds)
import qualified Database.Esqueleto as Q (Value (..), EntityField (..)) 
-- import Data.Int                     ( Int64 )

import Data.Time.Clock (nominalDay)
import Data.List (sort, maximumBy)
import Data.Text.Format (fixed)
import Data.List.Unique (allUnique)



-- type alias



anonymousUser = toSqlKey 1 :: UserId

-- data ZoneName =
--     Name String
--     | Offset Int

type TimeZoneHour = Int
type Millis = Int
type Minutes = Int



-- DATA DECLARATION



data ElmSchedule = ElmSchedule
    { elmScheduleBegin  :: Millis
    , elmScheduleEnd    :: Millis
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmSchedule)

data ElmUser = ElmUser
    { elmUserId         :: Int
    , elmUserName       :: Text
    , elmUserAdmin      :: Bool
    , elmUserDefaultDpy :: Maybe Int
    , elmUserZoneName   :: Maybe Text
    , elmUserZoneOffset :: Maybe Minutes
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmUser)

data ElmTask = ElmTask
    { elmTaskId         :: Int
    , elmTaskIsDummy    :: Bool
    , elmTaskIsDone     :: Bool
    , elmTaskIsStarred  :: Bool
    , elmTaskTitle      :: Maybe Text
    , elmTaskLink       :: Maybe Text
    , elmTaskStartable  :: Maybe Millis
    -- , elmTaskBegin      :: Maybe Millis
    -- , elmTaskEnd        :: Maybe Millis
    , elmTaskDeadline   :: Maybe Millis
    , elmTaskWeight     :: Maybe Double
    , elmTaskUser       :: Text
    , elmTaskSchedule   :: [ElmSchedule]
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmTask)

data ElmMessage = ElmMessage
    { elmMessageCode  :: Int
    , elmMessageBody  :: Text
    }

$(deriveJSON defaultOptions ''ElmMessage)

data ElmSubModel = ElmSubModel
    { elmSubModelUser       :: ElmUser
    , elmSubModelTasks      :: [ElmTask]
    , elmSubModelInputText  :: Maybe Text
    , elmSubModelMessage    :: Maybe ElmMessage
    }

$(deriveJSON defaultOptions ''ElmSubModel)

data TextPost = TextPost
    { textPostUser      :: ElmUser
    , textPostContent   :: Text
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''TextPost)

data UserSelTasks = UserSelTasks
    { userSelTasksUser :: ElmUser
    , userSelTasksIds  :: [Int]
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''UserSelTasks)

data UserTaskId = UserTaskId
    { userTaskIdUser :: ElmUser
    , userTaskIdTaskId :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''UserTaskId)



-- API DEFINITION



type API =  "dev"   :> "model"  :> Capture "user"  Int          :> Get  '[JSON] ElmSubModel
    :<|>    "tasks" :> "init"   :> ReqBody '[JSON] ElmUser      :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "post"   :> ReqBody '[JSON] TextPost     :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "done"   :> ReqBody '[JSON] UserSelTasks :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "star"   :> ReqBody '[JSON] UserTaskId   :> Post '[JSON] ()
    :<|>    "tasks" :> "focus"  :> ReqBody '[JSON] UserTaskId   :> Post '[JSON] [ElmTask]
    :<|>    "tasks" :> "home"   :> ReqBody '[JSON] ElmUser      :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "clone"  :> ReqBody '[JSON] UserSelTasks :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "arch"   :> ReqBody '[JSON] ElmUser      :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "trunk"  :> ReqBody '[JSON] ElmUser      :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "buds"   :> ReqBody '[JSON] ElmUser      :> Post '[JSON] ElmSubModel

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
    :<|> goHome
    :<|> cloneTasks
    :<|> showArchives
    :<|> showTrunk
    :<|> showBuds
    where
        devSubModel :: Int -> Handler ElmSubModel
        devSubModel = liftIO . devSubModel'
        initialize :: ElmUser -> Handler ElmSubModel
        initialize = liftIO . initialize'
        textPostReload :: TextPost -> Handler ElmSubModel
        textPostReload = liftIO . textPostReload'
        doneTasksReload :: UserSelTasks -> Handler ElmSubModel
        doneTasksReload = liftIO . doneTasksReload'
        switchStar :: UserTaskId -> Handler ()
        switchStar = liftIO . switchStar'
        focusTask :: UserTaskId -> Handler [ElmTask]
        focusTask = liftIO . focusTask'
        goHome :: ElmUser -> Handler ElmSubModel
        goHome = liftIO . goHome'
        cloneTasks :: UserSelTasks -> Handler ElmSubModel
        cloneTasks = liftIO . cloneTasks'
        showArchives :: ElmUser -> Handler ElmSubModel
        showArchives = liftIO . showArchives'
        showTrunk :: ElmUser -> Handler ElmSubModel
        showTrunk = liftIO . showTrunk'
        showBuds :: ElmUser -> Handler ElmSubModel
        showBuds = liftIO . showBuds'

devSubModel' :: Int -> IO ElmSubModel
devSubModel' uid = do
    pool <- pgPool
    user <- Prelude.head <$> getUserById pool uid
    durations <- getDurationsByUserId pool uid
    let elmUser = toElmUser user durations
    elmTasks <- getUndoneElmTasks uid
    return $ ElmSubModel elmUser elmTasks Nothing Nothing

initialize' :: ElmUser -> IO ElmSubModel
initialize' elmUser = do
    let uid = elmUserId elmUser
    devSubModel' uid

getUndoneElmTasks :: Int -> IO [ElmTask]
getUndoneElmTasks = 
    buildElmTasksByUserId  -- TODO
-- getUndoneElmTasks uid = do
--     pool <- pgPool
--     taskAssigns <- getUndoneTaskAssigns pool uid
--     return $ map toElmTask taskAssigns 

getMaxNode :: IO (Maybe Int)
getMaxNode = do
    pool <- pgPool
    pairs <- getMaxNode' pool
    let pair = case pairs of
            []      -> (Q.Value Nothing, Q.Value Nothing)
            (p:_)   -> p
    let mt = Q.unValue $ fst pair
    let mi = Q.unValue $ snd pair
    return $ max <$> mt <*> mi

textPostReload' :: TextPost -> IO ElmSubModel
textPostReload' (TextPost elmUser text) = do
    pool <- pgPool
    let uid = elmUserId elmUser
    user <- Prelude.head <$> (map entityVal) <$> getUserById pool uid
    durations <- (map entityVal) <$> getDurationsByUserId pool uid
    let tasksMaybeIds = tasksFromText user durations text
    fault <- faultPost user (map fst tasksMaybeIds)
    case fault of
        Just errMsg ->
            return $ ElmSubModel elmUser [] Nothing (Just errMsg) 
        Nothing -> do
            mn <- getMaxNode
            let maxNode = case mn of
                    Nothing -> 0
                    Just n  -> n
            let shift = maxNode + 2
            prediction <- insertOrUpdate tasksMaybeIds
            case prediction of
                Left errMsg' ->
                    return $ ElmSubModel elmUser [] Nothing (Just $ ElmMessage 400 errMsg')
                Right (inserts, updates, preds) -> do
                    maybeUpdate updates
                    let inserts' = preShift preds shift inserts 
                    insTasks . (shiftTaskNodes shift) $ inserts'
                    elmTasks <- getUndoneElmTasks uid
                    let ElmMessage _ ok1 = buildOkMsg inserts' " tasks registered."
                    let ElmMessage _ ok2 = buildOkMsg updates " tasks updated."
                    let okMsg = ElmMessage 200 (intercalate " " [ok1, ok2])
                    return $ ElmSubModel elmUser elmTasks Nothing (Just okMsg)

data Predictor =    PredTerm { predTermTarget :: Node, predTermToBe :: Node }
                |   PredInit { predInitTarget :: Node, predInitToBe :: Node }

insertOrUpdate :: [(Task, Maybe Int)] -> IO (Either Text ([Task], [(Int,Task)], [Predictor]))
insertOrUpdate xs = 
    if  allUnique $ filter ((/=) Nothing) $ map snd xs then
        insertOrUpdate' xs (map fst xs) ([], [], [])
    else
        return $ Left "Duplicate use of #."


insertOrUpdate' :: [(Task, Maybe Int)] -> [Task] -> ([Task], [(Int,Task)], [Predictor]) -> IO (Either Text ([Task], [(Int,Task)], [Predictor]))
insertOrUpdate' [] _ out = return $ Right out
insertOrUpdate' ((ta,mid):tas) ref (ins, upd, pred) = case mid of
    Nothing ->
        insertOrUpdate' tas ref ((ta:ins), upd, pred)
    Just id -> do
        pool <- pgPool
        get  <- getMeByTaskId pool (keyFromId id :: TaskId)
        case get of
            [] -> 
                return $ Left (intercalate " " ["Task ID", pack (show id), "does not exist."])
            (e:_) -> do
                let Task t i _ _ _ _ _ _ _ _ _ = ta
                let Task x y _ _ _ _ _ _ _ _ _ = entityVal e
                isOldT <- isOldTrunk x
                if  
                    isNowTrunk t ref then
                        insertOrUpdate' tas ref (ins, ((id,ta):upd), ((PredTerm i y):pred))
                else if
                    isNowBud i ref && isOldT then
                        insertOrUpdate' tas ref (ins, ((id,ta):upd), ((PredInit t x):pred))
                else
                    return $ Left (intercalate "" ["Invalid use of #", pack (show id), "."])
                
isOldTrunk :: Node -> IO (Bool)
isOldTrunk x = do
    pool <- pgPool
    allTrunk <- (map Q.unValue) <$> getAllTrunkNode pool
    return $ x `elem` allTrunk

maybeUpdate :: [(Int,Task)] -> IO ()
maybeUpdate updates = do
    pool <- pgPool
    sequence_ $ map (maybeUpdate' pool) updates

maybeUpdate' :: ConnectionPool -> (Int,Task) -> IO ()
maybeUpdate' pool (tid, task)
    | d /= False    = do
        setTaskDone pool tid
        maybeUpdate' pool (tid, Task t i y False s ml ms md mw mt u )
    | s /= False    = do
        setTaskStarred pool tid
        maybeUpdate' pool (tid, Task t i y d False ml ms md mw mt u )
    | ml /= Nothing = do
        let (Just l) = ml
        setTaskLink pool tid l
        maybeUpdate' pool (tid, Task t i y d s Nothing ms md mw mt u)
    | ms /= Nothing = do
        let (Just ss) = ms
        setTaskStartable pool tid ss
        maybeUpdate' pool (tid, Task t i y d s ml Nothing md mw mt u)
    | md /= Nothing = do
        let (Just dd) = md
        setTaskDeadline pool tid dd
        maybeUpdate' pool (tid, Task t i y d s ml ms Nothing mw mt u)
    | mw /= Nothing = do
        let (Just w) = mw
        setTaskWeight pool tid w
        maybeUpdate' pool (tid, Task t i y d s ml ms md Nothing mt u)
    | mt /= Nothing = do
        let (Just tt) = mt
        setTaskTitle pool tid tt
        maybeUpdate' pool (tid, Task t i y d s ml ms md mw Nothing u)
    | u /= anonymousUser = do
        setTaskUser pool tid u
        maybeUpdate' pool (tid, Task t i y d s ml ms md mw mt anonymousUser)
    | otherwise = 
        return ()
    where
        Task t i y d s ml ms md mw mt u = task

faultPost :: User -> [Task] -> IO (Maybe ElmMessage)
faultPost user tasks = do
    faultPerms <- faultPostPermission user tasks
    faultFormat <- faultPostFormat tasks
    let faultList =  [ faultPerms
                    , faultFormat
                    ]
    if Prelude.all (== Nothing) faultList then
        return Nothing
    else do
        let errMsg = buildPostErrMsg faultList
        return $ Just errMsg

faultPostPermission :: User -> [Task] -> IO (Maybe Text)
faultPostPermission user tasks = do
    -- TODO
    return Nothing

faultPostFormat :: [Task] -> IO (Maybe Text)
faultPostFormat tasks = do
    -- TODO
    return $ Nothing

doneTasksReload' :: UserSelTasks -> IO ElmSubModel
doneTasksReload' (UserSelTasks elmUser tids) = do
    pool <- pgPool
    let uid = elmUserId elmUser
    fault <- faultEditPermission uid tids
    case fault of
        Just errMsg ->
            return $ ElmSubModel elmUser [] Nothing (Just errMsg) 
        Nothing -> do
            sequence_ ( map (setTaskDoneOrUndone pool) tids)
            re <- reschedule uid
            case re of
                Left errMsg' ->
                    return $ ElmSubModel elmUser [] Nothing (Just $ ElmMessage 400 errMsg') 
                _ -> do
                    elmTasks <- getUndoneElmTasks uid
                    let okMsg = buildOkMsg tids " tasks done/undone."
                    return $ ElmSubModel elmUser elmTasks Nothing (Just okMsg)

switchStar' :: UserTaskId -> IO ()
switchStar' (UserTaskId elmUser tid) = do
    pool <- pgPool
    -- TODO check fault
    setStarSwitched pool tid

focusTask' :: UserTaskId -> IO [ElmTask]
focusTask' (UserTaskId elmUser tid) = do
    pool        <- pgPool
    -- TODO check fault
    before  <- map entityKey <$> getBeforeMeByTaskId pool (keyFromId tid :: TaskId)
    me      <- map entityKey <$> getMeByTaskId pool (keyFromId tid :: TaskId)
    after   <- map entityKey <$> getAfterMeByTaskId pool (keyFromId tid :: TaskId)
    let tids = Prelude.concat [before, me, after]
    elmTasks <- sequence (map buildElmTaskByTaskId tids) 
    return $ elmTasks

goHome' :: ElmUser -> IO ElmSubModel
goHome' elmUser = do
    let uid = elmUserId elmUser
    devSubModel' uid

cloneTasks' :: UserSelTasks -> IO ElmSubModel
cloneTasks' (UserSelTasks elmUser tids) = do
    pool <- pgPool
    let uid = elmUserId elmUser
    fault <- faultEditPermission uid tids
    case fault of
        Just errMsg ->
            return $ ElmSubModel elmUser [] Nothing (Just errMsg) 
        Nothing -> do
            user <- Prelude.head <$> (map entityVal) <$> getUserById pool uid
            taskAssigns <- (map (\(e,v) -> (entityVal e, (entityKey e, Q.unValue v)))) <$> getTaskAssignsByIds tids
            let text = textFromTasks user taskAssigns
            let okMsg = buildOkMsg taskAssigns " tasks cloned."
            return $ ElmSubModel elmUser [] (Just text) (Just okMsg)

faultEditPermission :: Int -> [Int] -> IO (Maybe ElmMessage)
faultEditPermission uid [] = 
    return Nothing
faultEditPermission uid (t:ts) = do
    perm <- hasEditPerm uid t
    if perm then
        faultEditPermission uid ts
    else do
        pool <- pgPool
        taskAssign <- Prelude.head <$> getTaskAssignById pool t
        return $ Just (buildEditErrMsg taskAssign)

getTaskAssignsByIds :: [Int] -> IO [(Entity Task, Q.Value Text)]
getTaskAssignsByIds ids = do
    pool <- pgPool
    Prelude.concat <$> sequence ( map (getTaskAssignById pool) ids)
    
hasEditPerm :: Int -> Int -> IO (Bool)
hasEditPerm uid tid =
    -- TODO
    return True 

showArchives' :: ElmUser -> IO ElmSubModel
showArchives' elmUser = do
    pool <- pgPool
    let uid = elmUserId elmUser
    -- TODO check fault
    tids <- map entityKey <$> getDoneTasksByUserId pool uid
    elmTasks <- sequence (map buildElmTaskByTaskId tids) 
    let okMsg = buildOkMsg elmTasks " archives here."
    return $ ElmSubModel elmUser elmTasks Nothing (Just okMsg) 

showTrunk' :: ElmUser -> IO ElmSubModel
showTrunk' elmUser = do
    pool <- pgPool
    let uid = elmUserId elmUser
    -- TODO check fault
    tids <- map entityKey <$> getTrunkTasksByUserId pool uid
    elmTasks <- sequence (map buildElmTaskByTaskId tids) 
    let okMsg = buildOkMsg elmTasks " trunk here."
    return $ ElmSubModel elmUser elmTasks Nothing (Just okMsg) 

showBuds' :: ElmUser -> IO ElmSubModel
showBuds' elmUser = do
    pool <- pgPool
    let uid = elmUserId elmUser
    -- TODO check fault
    tids <- map entityKey <$> getBudTasksByUserId pool uid
    elmTasks <- sequence (map buildElmTaskByTaskId tids) 
    let okMsg = buildOkMsg elmTasks " buds here."
    return $ ElmSubModel elmUser elmTasks Nothing (Just okMsg) 

reschedule :: Int -> IO (Either Text ())
reschedule uid = do
    pool <- pgPool
    user <- Prelude.head <$> map entityVal <$> getUserById pool uid
    now <- getCurrentTime
    durs <- map entityVal <$> getDurationsByUserId pool uid
    tasks <- getUndoneOwnTasksByUserId pool uid
    let sch =
            if userIsLazy user then Left "Now implementing"  --TODO
            else scheduleForward user now durs tasks
    case sch of
        Left errMsg ->
            return $ Left errMsg
        Right schedules -> do
            resetSchedules uid schedules
            return $ Right ()
            -- newTasks <- buildElmTasksByUserId uid
            -- let newTasks = attachSchedule tasks schedule
            -- let okMsg = buildOkMsg newTasks " tasks rescheduled."
            -- return $ ElmSubModel elmUser newTasks Nothing (Just okMsg) 

buildElmTaskByTaskId :: Key Task -> IO ElmTask
buildElmTaskByTaskId tid = do
    pool <- pgPool
    -- TODO check fault
    taskAssign <- Prelude.head <$> getTaskAssignByTaskId pool tid
    schedules <- map entityVal <$> getSchedulesByTaskId pool tid
    return $ toElmTask taskAssign schedules

buildElmTasksByUserId :: Int -> IO [ElmTask] 
buildElmTasksByUserId uid = do
    pool <- pgPool
    -- TODO check fault
    tids <- map entityKey <$> getUndoneTasksByUserId pool uid
    sequence $ map (buildElmTaskByTaskId) tids

resetSchedules :: Int -> [Schedule] -> IO ()
resetSchedules uid schedules = do
    pool <- pgPool
    delSchedulesByUserId pool uid
    insSchedules pool schedules




-- INTERNAL OPERATIONS



type Graph = [Edge]
type Edge = ((Node, Node), [Attr])
type Node  = Int
data Attr  = 
      AttrTaskId    { attrTaskId    :: Int  }
    | IsDone
    -- | IsDone        { isDone        :: Char }
    | IsStarred
    -- | IsStarred     { isStarred     :: Char }
    | HeadLink      { headLink      :: Text }
    | Title         { title         :: Text }
    | StartableDate { startableYea  :: Int, startableMon    :: Int, startableDay    :: Int }
    | StartableTime { startableHou  :: Int, startableMin    :: Int }
    | Weight        { weight        :: Double }
    | DeadlineDate  { deadlineYea   :: Int, deadlineMon     :: Int, deadlineDay     :: Int }
    | DeadlineTime  { deadlineHou   :: Int, deadlineMin     :: Int }
    | Assign        { assign        :: Text }
    | TailLink      { tailLink      :: Text }
    | Link          { link          :: Text }
    deriving (Eq, Show, Ord)

tasksFromText :: User -> [Duration] -> Text -> [(Task, Maybe Int)]
tasksFromText u ds = 
    (tasksFromGraph u ds) . graphFromText

tasksFromGraph :: User -> [Duration] -> Graph -> [(Task, Maybe Int)]
tasksFromGraph u ds g = 
    let
        firsts = 
            (map (universalize u)) . (map fst) . (map taskFromEdge) $ g
            -- (map (universalize u)) . (setBeginEnd u ds) . (map fst) . (map taskFromEdge) $ g
        seconds =
            (map snd) . (map taskFromEdge) $ g
    in
        zip firsts seconds

taskFromEdge :: Edge -> (Task, Maybe Int)
-- keep tid info
taskFromEdge ((t,i),as) =
    taskFromEdge' as ((Task t i False False False Nothing Nothing Nothing Nothing Nothing anonymousUser), Nothing)

taskFromEdge' :: [Attr] -> (Task, Maybe Int) -> (Task, Maybe Int)
taskFromEdge' [] task = 
    task
taskFromEdge' (a:as) ((Task t i y d s ml ms md mw mt u), mid) =
    case a of
        AttrTaskId n ->
            taskFromEdge' as ((Task t i y d s ml ms md mw mt u), Just n)
        IsDone ->
            taskFromEdge' as ((Task t i y True s ml ms md mw mt u), mid)
        IsStarred ->
            taskFromEdge' as ((Task t i y d True ml ms md mw mt u), mid)
        Link l ->
            taskFromEdge' as ((Task t i y d s (Just l) ms md mw mt u), mid)
        StartableDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case ms of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as ((Task t i y d s ml (Just (UTCTime nd ot)) md mw mt u), mid)
                    Nothing ->
                        taskFromEdge' as ((Task t i y d s ml (Just (UTCTime nd 0)) md mw mt u), mid)
        StartableTime hh mm ->
            let
                nt = secondsToDiffTime . fromIntegral $ 60 * (mm + 60 * hh)
            in
                case ms of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as ((Task t i y d s ml (Just (UTCTime od nt)) md mw mt u), mid)
                    Nothing ->
                        let
                            nd = fromGregorian 3000 0 0
                        in
                            taskFromEdge' as ((Task t i y d s ml (Just (UTCTime nd nt)) md mw mt u), mid)
        DeadlineDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case md of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as ((Task t i y d s ml ms (Just (UTCTime nd ot)) mw mt u), mid)
                    Nothing ->
                        taskFromEdge' as ((Task t i y d s ml ms (Just (UTCTime nd 0)) mw mt u), mid)
        DeadlineTime hh mm ->
            let
                nt = secondsToDiffTime . fromIntegral $ 60 * (mm + 60 * hh)
            in
                case md of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as ((Task t i y d s ml ms (Just (UTCTime od nt)) mw mt u), mid)
                    Nothing ->
                        let
                            nd = fromGregorian 3000 0 0
                        in
                            taskFromEdge' as ((Task t i y d s ml ms (Just (UTCTime nd nt)) mw mt u), mid)
        Weight w ->
            taskFromEdge' as ((Task t i y d s ml ms md (Just w) mt u), mid)
        -- Assign an ->  --TODO
        Title tt' ->
            case mt of
                Just tt ->
                    taskFromEdge' as ((Task t i y d s ml ms md mw (Just $ Data.Text.intercalate " " [tt', tt]) u), mid)
                Nothing ->
                    taskFromEdge' as ((Task t i y d s ml ms md mw (Just tt') u), mid)
        _ -> 
            taskFromEdge' as ((Task t i y d s ml ms md mw mt u), mid)

graphFromText :: Text -> Graph
graphFromText = spanDummy . assemble . markUp . chopLines

type Indent = Int
indent = "    " :: Text

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
    <|> IsDone        <$  char '%'
    <|> IsStarred     <$  char '*'
    <|> Link          <$  char '&'  <*> takeText
    <|> StartableDate <$> decimal   <*  char '/' <*> decimal <* char '/' <*> decimal <* char '-'
    <|> StartableTime <$> decimal   <*  char ':' <*> decimal <* char '-'
    <|> DeadlineDate  <$  char '-'  <*> decimal  <* char '/' <*> decimal <* char '/' <*> decimal
    <|> DeadlineTime  <$  char '-'  <*> decimal  <* char ':' <*> decimal
    <|> Weight        <$  char '$'  <*> double
    -- <|> HeadLink      <$> takeTill (==']') <* char ']'
    <|> HeadLink      <$  char ']'  <*> takeText
    <|> TailLink      <$  char '['  <*> takeText
    <|> Assign        <$  char '@'  <*> takeText
    <|> Title         <$> takeText

assemble  :: [((Node, Node), [Text])] -> Graph
assemble = map $ assemble' []

assemble' :: [Attr] -> ((Node, Node), [Text]) -> Edge
assemble' mem ((t,i), []) = ((t,i), mem)
assemble' mem ((t,i), a:as)  = 
    case parseOnly aAttr a of
        Left _ -> ((t,i), mem)
        Right r -> assemble' (r:mem) ((t,i), as)

spanDummy :: Graph -> Graph
-- TODO
spanDummy g = spanDummy' g g g

spanDummy' :: [Edge] -> [Edge] -> Graph -> Graph
spanDummy' [] _ g                    = g
spanDummy' (t:ts) [] g               = spanDummy' ts g g
spanDummy' ((p,at):ts) ((q,ah):hs) g = 
    if keyMatch at ah ah then
        spanDummy' ((p,at):ts) hs (spanDummy'' p q g)
    else
        spanDummy' ((p,at):ts) hs g

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

spanDummy'' :: (Node, Node) -> (Node, Node) -> Graph -> Graph
spanDummy'' (_,t) (i,_) g = ((t,i), [AttrTaskId 0, IsDone, Title "DUMMY"]) : g

universalize :: User -> Task -> Task
universalize user = timeShift minus
    where
        minus = 60*60*(-1)*(fromIntegral $ userTimeZone user)

localize :: User -> Task -> Task
localize user = timeShift plus
    where
        plus = 60*60*(fromIntegral $ userTimeZone user)

timeShift :: NominalDiffTime -> Task -> Task
timeShift diff (Task t i y d s l ms md mw tt u) = 
    let
        msS = addUTCTime <$> Just diff <*> ms
        mdS = addUTCTime <$> Just diff <*> md
    in
        Task t i y d s l msS mdS mw tt u

type AssignName = Text

textFromTasks :: User -> [(Task, (TaskId, AssignName))] -> Text
textFromTasks u =
    textFromGraph . (graphFromTasks u)

graphFromTasks :: User -> [(Task, (TaskId, AssignName))] -> Graph
graphFromTasks u =  (map edgeFromTask) . (localize' u)

localize' :: User -> [(Task, (TaskId, AssignName))] -> [(Task, (TaskId, AssignName))]
localize' u =
    map (\(t,(i,a)) -> (localize u t,(i,a)))

edgeFromTask :: (Task, (TaskId, AssignName)) -> Edge
edgeFromTask (task, (id,an)) =
    let
        attrs   =   [ AttrTaskId (fromIntegral . fromSqlKey $ id)
                    , Assign an
                    ]
        attrs'  =   edgeFromTask' task attrs
    in
        ((taskTerminal task, taskInitial task), attrs')

edgeFromTask' :: Task -> [Attr] -> [Attr]
edgeFromTask' (Task t i y d s ml ms md mw mt u) result
    | y =
        edgeFromTask' (Task t i False d s ml ms md mw mt u) ((AttrTaskId 0) : result)
    | d =
        edgeFromTask' (Task t i False False s ml ms md mw mt u) (IsDone : result)
    | s =
        edgeFromTask' (Task t i False False False ml ms md mw mt u) (IsStarred : result)
    | ml /= Nothing =
        let
            Just l = ml
        in
            edgeFromTask' (Task t i False False False Nothing ms md mw mt u) ((Link l) : result)
    | ms /= Nothing =
        let
            Just ss = ms
            UTCTime date clock = ss
            (yea, mon, day) = toGregorian date
            (hou, min, _) = toHMS clock
            sD = StartableDate (fromIntegral yea) mon day
            sT = StartableTime hou min
        in
            edgeFromTask' (Task t i False False False Nothing Nothing md mw mt u) (sD : sT : result)
    | md /= Nothing =
        let
            Just dd = md
            UTCTime date clock = dd
            (yea, mon, day) = toGregorian date
            (hou, min, _) = toHMS clock
            dD = DeadlineDate (fromIntegral yea) mon day
            dT = DeadlineTime hou min
        in
            edgeFromTask' (Task t i False False False Nothing Nothing Nothing mw mt u) (dD : dT : result)
    | mw /= Nothing =
        let
            Just w = mw
        in
            edgeFromTask' (Task t i False False False Nothing Nothing Nothing Nothing mt u) ((Weight w) : result)
    | mt /= Nothing =
        let
            Just tt = mt
        in
            edgeFromTask' (Task t i False False False Nothing Nothing Nothing Nothing Nothing u) ((Title tt) : result)
    | otherwise =
        result

toHMS :: DiffTime -> (Int, Int, Int)
toHMS d =
    let
        sec = fromIntegral $ (diffTimeToPicoseconds d) `div` (10^12) :: Int
        (h,rem) = ((sec `div` 3600), (sec `mod` 3600))
        (m, s) = ((rem `div` 60), (rem `mod` 60))
    in
        (h, m, s)

textFromGraph :: Graph -> Text
textFromGraph =
    bindLines . markDown . dissemble . asideDummy

asideDummy :: Graph -> Graph 
asideDummy = 
    eraseDummy . remDupLink . addLink

maxNode :: Graph -> Node -> Node
maxNode [] entry = entry
maxNode (((t,i),_):es) entry = maxNode es (max entry (max t i))

addLink :: Graph -> Graph
addLink g = addLink' (maxNode g 0) g g g

addLink' :: Node -> Graph -> Graph -> Graph -> Graph
-- CAUTION: cause infinite loop for negative node
addLink' (-1) _ _ result =
    result
addLink' node [] origin result =
    addLink' (node - 1) origin origin result
addLink' node (((t,i),as):es) origin result
    | node == i =
        if hasDummy as then
            addLink' (node - 1) origin origin ((addHead node node) . (addTail t node) $ result)
        else
            addLink' node es origin result
    | node == t =
        if hasDummy as then
            addLink' (node - 1) origin origin ((addHead i node) . (addTail node node) $ result)
        else
            addLink' node es origin result
    | otherwise =
        addLink' node es origin result

addTail :: Node -> Node -> Graph -> Graph
addTail initial key =
    map (\((t,i),as) ->
        if i == initial then ((t,i),((TailLink (pack $ show key)):as)) else ((t,i),as))

addHead :: Node -> Node -> Graph -> Graph
addHead terminal key =
    map (\((t,i),as) ->
        if t == terminal then ((t,i),((HeadLink (pack $ show key)):as)) else ((t,i),as))

hasDummy :: [Attr] -> Bool
hasDummy =
    Prelude.any (\attr -> attr == AttrTaskId 0)

remDupLink :: Graph -> Graph
remDupLink =  map (\(p, as) -> (p, remDupLink' as as))

remDupLink' :: [Attr] -> [Attr] -> [Attr]
remDupLink' [] result = result
remDupLink' ((TailLink key):as) result =
    remDupLink' as ((TailLink key) : (filter (\a -> a /= TailLink key) result))
remDupLink' ((HeadLink key):as) result =
    remDupLink' as ((HeadLink key) : (filter (\a -> a /= HeadLink key) result))
remDupLink' (_:as) result =
    remDupLink' as result

eraseDummy :: Graph -> Graph
eraseDummy = 
    filter (\(_,as) -> not $ hasDummy as)

dissemble :: Graph -> [((Node, Node), Text)]
dissemble =
    map (\((t,i), as) -> ((t,i), wordsFromAttrs as))

wordsFromAttrs :: [Attr] -> Text
wordsFromAttrs = 
    Data.Text.unwords . (map (LT.toStrict . B.toLazyText . wordsFromAttrs')) . Data.List.sort

wordsFromAttrs' :: Attr -> B.Builder
wordsFromAttrs' a = case a of
    AttrTaskId    i     -> B.singleton '#' <> B.decimal i
    IsDone              -> B.singleton '%'
    IsStarred           -> B.singleton '*'
    HeadLink      h     -> B.singleton ']' <> B.fromText h
    Title         t     -> B.fromText t
    StartableDate y m d -> 
        B.decimal y <>  B.singleton '/' <> B.decimal m <> B.singleton '/' <> B.decimal d <> B.singleton '-'
    StartableTime h m ->
        B.decimal h <>  B.singleton ':' <> B.decimal m <> B.singleton '-'
    Weight        w     -> B.singleton '$' <> fixed 1 w
    DeadlineDate  y m d ->
        B.singleton '-' <> B.decimal y <>  B.singleton '/' <> B.decimal m <> B.singleton '/' <> B.decimal d
    DeadlineTime  h m ->
        B.singleton '-' <> B.decimal h <>  B.singleton ':' <> B.decimal m
    Assign        a     -> B.singleton '@' <> B.fromText a
    TailLink      t     -> B.singleton '[' <> B.fromText t
    Link          l     -> B.singleton '&' <> B.fromText l

markDown :: [((Node, Node), Text)] -> [(Indent, Text)]
markDown xs =
    let
        tree'   = renumberTrunk (-1) (map fst xs)
        xs'     = zip tree' (map snd xs)
        ys      = markDown' tree'
    in
        mergeOnNodes ys xs'

mergeOnNodes :: [((Node, Node), Indent)] -> [((Node, Node), Text)] -> [(Indent, Text)]
mergeOnNodes ys xs = mergeOnNodes' ys xs xs []

mergeOnNodes' :: [((Node, Node), Indent)] -> [((Node, Node), Text)] -> [((Node, Node), Text)] -> [(Indent, Text)] -> [(Indent, Text)]
mergeOnNodes' [] _ _ result = result
mergeOnNodes' ys [] xOrigin result = mergeOnNodes' ys xOrigin xOrigin result
mergeOnNodes' (((t,i),ind):ys) (((u,j),tex):xs) xOrigin result
    | (t,i) == (u,j)    = mergeOnNodes' ys xOrigin xOrigin ((ind,tex):result)
    | otherwise         = mergeOnNodes' (((t,i),ind):ys) xs xOrigin result

isTrunkNode :: [(Node, Node)] -> Node -> Bool 
isTrunkNode tree node =
    Prelude.null (prevNodes node tree)

isBudNode :: [(Node, Node)] -> Node -> Bool 
isBudNode tree node =
    Prelude.null (nextNodes node tree)

renumberTrunk :: Node -> [(Node, Node)] -> [(Node, Node)]
renumberTrunk n tree =
    map (\(t,i) -> if isTrunkNode tree t then (n,i) else (t,i)) tree

markDown' :: [(Node, Node)] -> [((Node, Node), Indent)]
markDown' tree = 
    let
        root = -1
    in
        markDown'' root 0 [] (nextNodes root tree) tree tree []

markDown'' :: Node -> Int -> [Node] -> [Node] -> [(Node, Node)] -> [(Node, Node)] -> [((Node, Node), Indent)] -> [((Node, Node), Indent)]
markDown'' _ _ [] [] _ _ out = out
markDown'' node depth remain (x:xs) tre tree out =
    let
        next = nextNodes x tre
        tre' = explored x node tre
    in
        markDown'' x (depth + 1) (node : remain) next tre' tree (((node, x), depth) : out)
markDown'' node depth (r:rs) [] tre tree out =
    let
        prev = Prelude.head $ prevNodes node tree
        next = nextNodes prev tre
    in
        if prev == r then
            markDown'' r (depth - 1) rs next tre tree out
        else
            markDown'' prev (depth - 1) (r:rs) next tre tree out

nextNodes :: Node -> [(Node, Node)] -> [Node]
nextNodes x =
    (map snd) . (filter (\(t,i) -> t == x))

prevNodes :: Node -> [(Node, Node)] -> [Node]
prevNodes node =
    (map fst) . (filter (\(t,i) -> i == node))

explored :: Node -> Node -> [(Node, Node)] -> [(Node, Node)]
explored x node =
    filter (\(t,i) -> (t,i) /= (node, x))

bindLines :: [(Indent, Text)] -> Text
bindLines =
    Data.Text.unlines . map (\(i,t) -> Data.Text.concat [Data.Text.replicate i indent, t])

idFromEntity :: (Integral a, ToBackendKey SqlBackend record) => Entity record -> a
idFromEntity = idFromKey . entityKey

idFromKey :: (Integral a, ToBackendKey SqlBackend record) => Key record -> a
idFromKey = fromIntegral . fromSqlKey

keyFromId :: (Integral a, ToBackendKey SqlBackend record) => a -> Key record
keyFromId = toSqlKey . fromIntegral

toElmTask :: (Entity Task, Q.Value Text) -> [Schedule] -> ElmTask
toElmTask (task,assignName) schedules =
    let
        i  = idFromEntity task
        Task _ _ y d s ml ms md mw mt _ = entityVal task
        ems = toElmTime ms
        emd = toElmTime md
        eu  = Q.unValue assignName
        esch = map toElmSchedule schedules
    in
        ElmTask i y d s mt ml ems emd mw eu esch

-- toElmTask :: (Entity Task, Q.Value Text) -> ElmTask
-- toElmTask (e, u) =
--     let
--         i  = idFromEntity e
--         Task _ _ y d s ml ms md mw mt _ = entityVal e
--         ems = toElmTime ms
--         emb = toElmTime mb
--         eme = toElmTime me
--         emd = toElmTime md
--         eu  = Q.unValue u
--     in
--         ElmTask i y d s mt ml ems emb eme emd mw eu

toElmUser :: Entity User -> [Entity Duration] -> ElmUser
toElmUser eu ers =
    let
        i = idFromEntity eu
        User n a _ _ _ mdpy _ _ _ = entityVal eu
    in
        ElmUser i n a mdpy Nothing Nothing

toElmTime :: Maybe UTCTime -> Maybe Millis
toElmTime Nothing = 
    Nothing
toElmTime (Just t) =
    Just (floor $ 10^3 * utcTimeToPOSIXSeconds t)

shiftTaskNodes :: Int -> [Task] -> [Task]
shiftTaskNodes sh ts =
    map (
        \(Task t i y d s l ms md w tt u) -> 
            let 
                (t',i') = both ((+) sh) (t,i)
            in
                Task t' i' y d s l ms md w tt u
        ) ts

timeZoneHour :: ElmUser -> TimeZoneHour
timeZoneHour _ = 9  -- TODO

-- setBeginEnd :: User -> [Duration] -> [Task] -> [Task]
-- setBeginEnd u ds = 
--     map (setBeginEnd' u ds)

-- setBeginEnd' :: User -> [Duration] -> Task -> Task
-- setBeginEnd' user ds (Task t i y d s ml ms md mw mt u) =
--     case mw of
--         Nothing ->
--             Task t i y d s ml ms md mw mt u
--         Just w ->
--             let
--                 weight = millisFromWeight w
--             in
--             if  userIsLazy user then
--                 let
--                     end = md
--                     begin = case end of
--                         Just e ->
--                             beginFromEnd e ds weight
--                         _ ->
--                             Nothing
--                 in
--                 Task t i y d s ml ms begin end md mw mt u 
                
--             else
--                 let
--                     begin = ms
--                     end = case begin of
--                         Just b ->
--                             endFromBegin b ds weight
--                         _ ->
--                             Nothing
--                 in
--                 Task t i y d s ml ms begin end md mw mt u 

millisFromWeight :: Double -> Millis
millisFromWeight w = floor $ 60 * 60 * 10^3 * w

type MillisDuration = (Millis, Millis)

toMillis :: Duration -> MillisDuration
toMillis (Duration l r _) = (fromTOD l, fromTOD r)
    where
        fromTOD :: TimeOfDay -> Millis
        fromTOD t = floor $ (10^3)*(todSec t)

millisFromUTC ::UTCTime -> Millis
millisFromUTC u = floor . ((*) (10^3)) . utcTimeToPOSIXSeconds $ u

utcFromMillis :: Millis -> UTCTime
utcFromMillis = posixSecondsToUTCTime . fromIntegral. (`div` 10^3)  

-- endFromBegin :: UTCTime -> [Duration] -> Millis -> Maybe UTCTime
-- endFromBegin begin ds weight =
--     let
--         ds' = map toMillis ds
--         begin' = millisFromUTC begin
--         diff = posi ds' ds' begin' weight
--     in
--     utcFromMillis <$> ((+) <$> (Just begin') <*> diff)

-- beginFromEnd :: UTCTime -> [Duration] -> Millis -> Maybe UTCTime
-- beginFromEnd end ds weight = 
--     let
--         ds' = reverse $ map toMillis ds
--         end' = millisFromUTC end
--         diff = posi' [] ds' end' weight
--     in
--     utcFromMillis <$> ((+) <$> (Just end') <*> diff)

-- millisPerDay :: Millis
-- millisPerDay = floor $ 10^3 * nominalDay

-- isInDur :: Millis -> MillisDuration -> Bool
-- isInDur t (l,r) =
--     l <= t && t<= r 

-- dayPlus :: [MillisDuration] -> [MillisDuration]
-- dayPlus = map (both (+ millisPerDay) )

-- posi :: [MillisDuration] -> [MillisDuration] -> Millis -> Millis -> Maybe Millis
-- posi  _ [] _ _ = 
--     Nothing
-- posi [] seed left rest =
--     posi (dayPlus seed) (dayPlus seed) left rest
-- posi (d:ds) seed left rest
--     | left `isInDur` d && (left + rest) `isInDur` d =
--         Just (left + rest)
--     | left `isInDur` d =
--         nega ds seed (snd d) (rest - (snd d - left)) 
--     | otherwise =
--         nega (d:ds) seed left rest

-- nega :: [MillisDuration] -> [MillisDuration] -> Millis -> Millis -> Maybe Millis
-- nega  _ [] _ _ = 
--     Nothing
-- nega [] seed left rest =
--     nega (dayPlus seed) (dayPlus seed) left rest
-- nega (d:ds) seed left rest
--     | left `isInDur` d =
--         posi (d:ds) seed left rest
--     | otherwise =
--         posi (d:ds) seed (fst d) rest

-- dayMinus :: [MillisDuration] -> [MillisDuration]
-- dayMinus = map (both (\t -> t - millisPerDay))

-- posi' :: [MillisDuration] -> [MillisDuration] -> Millis -> Millis -> Maybe Millis
-- posi' _ [] _ _ = 
--     Nothing
-- posi' [] seed right rest =
--     posi' (dayMinus seed) (dayMinus seed) right rest
-- posi' (d:ds) seed right rest
--     | right `isInDur` d && (right - rest) `isInDur` d =
--         Just (right - rest)
--     | right `isInDur` d =
--         nega' ds seed (fst d) (rest - (right - fst d)) 
--     | otherwise =
--         nega' (d:ds) seed right rest

-- nega' :: [MillisDuration] -> [MillisDuration] -> Millis -> Millis -> Maybe Millis
-- nega' _ [] _ _ = 
--     Nothing
-- nega' [] seed right rest =
--     nega' (dayMinus seed) (dayMinus seed) right rest
-- nega' (d:ds) seed right rest
--     | right `isInDur` d =
--         posi' (d:ds) seed right rest
--     | otherwise =
--         posi' (d:ds) seed (snd d) rest

buildOkMsg :: [a] -> String -> ElmMessage
buildOkMsg xs str =
    let body = pack $ (show $ Prelude.length xs) ++ str
    in  ElmMessage 200 body

buildPostErrMsg :: [Maybe Text] -> ElmMessage
buildPostErrMsg []              = ElmMessage 300 "No fault?"
buildPostErrMsg ((Just text):_) = ElmMessage 400 text
buildPostErrMsg (Nothing:es)  = buildPostErrMsg es

buildEditErrMsg :: (Entity Task, Q.Value Text) -> ElmMessage
buildEditErrMsg (_, Q.Value userName) =
    let body = Data.Text.concat ["No permission to edit ", userName, "'s tasks."]
    in  ElmMessage 400 body

isNowTrunk :: Node -> [Task] -> Bool
isNowTrunk t =
    Prelude.all (\task -> (taskInitial task) /= t)

isNowBud :: Node -> [Task] -> Bool
isNowBud i =
    Prelude.all (\task -> i /= (taskTerminal task))

preShift :: [Predictor] -> Int -> [Task] -> [Task]
preShift [] shift tasks = tasks
preShift ((PredTerm ii yy):ps) shift tasks =
    let
        targets  = filter (\task -> taskTerminal task == ii) tasks
        others   = filter (\task -> taskTerminal task /= ii) tasks
        targets' =
            map (\(Task t i y d s ml ms md mw mt u) -> 
                Task (yy-shift) i y d s ml ms md mw mt u) targets
    in
        preShift ps shift (Prelude.concat [targets', others])
preShift ((PredInit tt xx):ps) shift tasks =
    let
        targets  = filter (\task -> taskInitial task == tt) tasks
        others   = filter (\task -> taskInitial task /= tt) tasks
        targets' =
            map (\(Task t i y d s ml ms md mw mt u) -> 
                Task t (xx-shift) y d s ml ms md mw mt u) targets
    in
        preShift ps shift (Prelude.concat [targets', others])

toElmSchedule :: Schedule -> ElmSchedule
toElmSchedule (Schedule b e _) = 
    ElmSchedule (millisFromUTC b) (millisFromUTC e)

scheduleForward :: User -> UTCTime -> [Duration] -> [Entity Task] -> Either Text [Schedule]
scheduleForward user now ds tasks =
    if  hasLoop (map entityVal tasks) then
        Left "Not applicable to diagrams with loops."
    else
    let
        today = midnightBy now
        daily = map toMillis ds
        pattern = stripedPattern (map (both $ (+) today) daily)
        reso = 60 * (10^3) * (userResolutionMin user)
        -- queue = (queueOfPairs reso) . oneStrokes $ tasks
        -- roll = pianoRoll pattern queue reso (millisFromUTC now)
        cursor = millisFromUTC now
        frags = map toTaskFrag (map entityVal tasks)
        roll = pianoRollF pattern reso cursor frags []
    in
        Right $ toSchedule tasks roll

hasLoop :: [Task] -> Bool
hasLoop tasks =
    let 
        pairs = map pairFromTask tasks
        buds = filter (isBudNode pairs) (map snd pairs)
        goals = map (\b -> hasLoop' b (prevNodes b pairs) [] pairs) buds
    in
    Prelude.any (\g -> not $ isTrunkNode pairs g) goals

pairFromTask :: Task -> (Node, Node)
pairFromTask t = (taskTerminal t, taskInitial t)

hasLoop' :: Node -> [Node] -> [Node] -> [(Node, Node)] -> Node
-- TODO
hasLoop' current [] [] chart = 
    current
hasLoop' current (x:_) remain chart =
    hasLoop' x (prevNodes x chart) (current:remain) (filter ((/=) (x, current)) chart)
hasLoop' current [] (r:rs) chart  =
    hasLoop' r (prevNodes r chart) rs chart 

midnightBy :: UTCTime -> Millis
midnightBy t = millisFromUTC $ addUTCTime  (fromRational $ (-1)*(toRational $ utctDayTime t)) t

stripedPattern :: [MillisDuration] -> [MillisDuration]
stripedPattern ds =
    let oneDay = floor $ (10^3) * nominalDay
    in  Prelude.concat [ds, (map (both $ (+) oneDay) (stripedPattern ds))]
 
data TaskFrag = TaskFrag
    { fPair :: (Node, Node)
    , fStartable :: Maybe Millis
    , fDeadline :: Maybe Millis
    , fWeight :: Maybe Millis
    } deriving (Eq, Show)

toTaskFrag :: Task -> TaskFrag
toTaskFrag t =
    let
        pair = (taskTerminal t, taskInitial t)
        startable = millisFromUTC <$> (taskStartable t)
        deadline = millisFromUTC <$> (taskDeadline t)
        weight = millisFromWeight <$> (taskWeight t)
    in
        TaskFrag pair startable deadline weight

-- fragsFromNodes :: [Task] -> [Node] -> [TaskFrag]
-- fragsFromNodes ref nodes =
--     fragsFromNodes' ref nodes []
    
-- fragsFromNodes' :: [Task] -> [Node] -> [TaskFrag] -> [TaskFrag]
-- fragsFromNodes'  _ [] out =
--     out
-- fragsFromNodes'  _ (i:[]) out =
--     out
-- fragsFromNodes' ref (i:t:ns) out =
--     case findTaskByPair ref (t,i) of
--         Nothing ->
--             fragsFromNodes' ref (t:ns) out
--         Just task ->
--             let frag = toTaskFrag now task
--             in  fragsFromNodes' ref (t:ns) (frag:out)

-- StartDeadSeqs :: UTCTime -> [Task] -> [[TaskFrag]]
-- StartDeadSeqs now tasks =
--     let 
--         pairs = map pairFromTask tasks
--         seqs = map (fragsFromNodes now tasks) (oneStrokes pairs)
--         seqs' = filter 
--                     (\seq -> case seq of
--                         (frag:frags) -> 
--                             let TaskFrag (_,i) ms _ _ = frag
--                             in  isNowBud i tasks || ms /= Nothing
--                         - ->
--                             False
--                     ) seqs
--     in
--         map (\(frag:frags) -> (orSetStartableAsNow now frag):frags) seqs'

-- orSetStartableAsNow :: UTCTime -> TaskFrag -> TaskFrag
-- orSetStartableAsNow now frag =
--     case fStartable frag of
--         Just s -> 
--             frag
--         Nothing ->
--             let TaskFrag p ms md mw = frag
--             in  TaskFrag p (Just $ millisFromUTC now) md mw

-- oneStrokes :: [(Node, Node)] -> [[Node]]
-- oneStrokes pairs =
--     let trunks = filter (isTrunkNode pairs) (map fst pairs)
--     in  concatMap (\trunk -> oneStrokes' trunk (nextNodes trunk pairs) [] [] pairs [] []) trunks

-- oneStrokes' :: Node -> [Node] -> [Node] -> [(Node, Node)] -> [(Node, Node)] -> [Node] -> [[Node]] -> [[Node]]
-- oneStrokes' current next remain cha chart stack out
-- oneStrokes' _ [] [] _ _ _ out =
--     out
-- oneStrokes' current (x:xs) remain cha chart stack out =
--     oneStrokes' x (nextNodes x cha) (current:remain) (lightenNext cha chart current x) chart (x:stack) ((x:stack):out) 
-- oneStrokes' current [] (r:remain) cha chart stack out =
--     oneStrokes' r  (nextNodes r cha) remain cha chart (dropWhile (/= r) stack) out
--     where
--         lightenNext cha chart current x =
--             Prelude.concat [filter (/= (current,x)) cha, filter (== (x,_)) chart]

-- queueOfFrags :: Millis -> [[TaskFrag]] -> [(Node, Node)]
-- queueOfFrags reso seqs = 
--     reverse $ queueOfFrags' reso seqs []

-- queueOfFrags' :: Millis -> [[TaskFrag]] -> [TaskFrag] -> [TaskFrag]
-- queueOfFrags' reso seqs out
--     | targetSeqs == [] =
--         out
--     | otherwise =
--         queueOfFrags' reso seqsUpdated (crt : out)
--     where
--         targetSeqs = filter (\seq -> seqWeight seq > 0) seqs
--         crt = criticalFrag targetSeqs
--         TaskSeq qN qS qD qW = crt
--         seqsUpdated =  map (\q -> if q == crt then (TaskSeq qN qS qD (qW - reso)) else q) targetSeqs

-- seqsUpdated :: [TaskFrag] -> [[TaskFrag]] -> [[TaskFrag]]
-- seqsUpdated _ [] = 
--     []
-- seqsUpdated [] entire =
--     entire
-- seqsUpdated ((TaskFrag _ _ _ Nothing):_) entire =
--     entire
-- seqsUpdated ((TaskFrag _ _ _ (Just fw)):fs) entire =

-- weightDown :: Millis -> [TaskFrag] -> [TaskFrag]
-- weightDown w [] =
--     []
-- weightDown w ((TaskFrag _ _ _ Nothing):fs) 

-- seqStartable :: [TaskFrag] -> Millis

-- seqWeight seq

-- margin :: [TaskFrag]-> Millis
-- margin (TaskSeq _ qs qd qw) =
--     qd - (qs + qw)

-- criticalFrag :: [[TaskFrag]] -> TaskSeq
-- criticalFrag targetSeqs =
--     minimumBy (\p q -> compare (margin p) (margin q))

-- pianoRoll :: [MillisDuration] -> [(Node, Node)] -> Millis -> Millis -> [((Node, Node), MillisDuration)]
-- pianoRoll pattern queue reso startable =
--     pianoRoll' pattern queue reso startable reso []

-- pianoRoll' :: [MillisDuration] -> [TaskFrag] -> Millis -> Millis -> Millis -> [((Node, Node), MillisDuration)] -> [((Node, Node), MillisDuration)]
-- pianoRoll'_ [] _ _ _ out = out
-- pianoRoll'((l,r):(l',r'):pattern) (q:queue) reso left rest out
--     | left < fStartable q
--         pianoRoll'((l,r):(l',r'):pattern) (q:queue) reso (fStartable q) rest out
--     | l <= left < r && r < left + rest =
--         let
--             right = r
--             rest' = left + rest - r
--         in 
--             pianoRoll' ((l',r'):pattern) (q:queue) reso l' rest' ((q,(left,right)):out)
--     | l <= left < r =
--         let
--             right = left + rest
--         in
--             pianoRoll' ((l',r'):pattern) queue reso right reso ((q,(left,right)):out)
--     | left < l =
--         pianoRoll'((l,r):(l',r'):pattern) (q:queue) reso l rest out
--     | otherwise =
--         pianoRoll'((l,r):(l',r'):pattern) (q:queue) reso l' rest out

toSchedule :: [Entity Task] -> [(TaskFrag, [MillisDuration])] -> [Schedule]
toSchedule ts ss =
    toSchedule' ts ss []

toSchedule' :: [Entity Task] -> [(TaskFrag, [MillisDuration])] -> [Schedule] -> [Schedule]
toSchedule' [] _ _ =
    []
toSchedule' _ [] out =
    out
toSchedule' ts ((f,ds):ss) out =
    case findTaskByPair ts (fPair f) of
        Nothing ->
            toSchedule' ts ss out
        Just task ->
            let
                schedules = map (\(l,r) -> Schedule (utcFromMillis l) (utcFromMillis r) (entityKey task)) ds
            in  toSchedule' ts ss (Prelude.concat [schedules, out])

findTaskByPair :: [Entity Task] -> (Node, Node) -> Maybe (Entity Task)
findTaskByPair [] _ =
    Nothing
findTaskByPair (task:ts) p
    | p == (taskTerminal (entityVal task), taskInitial (entityVal task))  =
        Just task
    | otherwise =
        findTaskByPair ts p

pianoRollF :: [MillisDuration] -> Millis -> Millis -> [TaskFrag] -> [(TaskFrag, [MillisDuration])] -> [(TaskFrag, [MillisDuration])]
pianoRollF pattern reso cursor frags out 
    | Prelude.null $ filterW frags =
        out
    | Prelude.null $ filterS frags =
        pianoRollF pattern reso next frags out
    | otherwise =
        pianoRollF pattern reso next newFrags ((winner,reward):out)
    where
        filterW = 
            filter (hasDirectOrIndirectDeadline frags) .
            filter (hasWeight)
        filterS =
            filter (isExecutable cursor frags) .
            filterW
        entry =
            filterS frags
        winner =
            maximumBy (\f g -> compare (urgency cursor frags f) (urgency cursor frags g)) entry
        reward =
            assignTime pattern reso cursor
        next =
            Prelude.maximum $ map snd reward
        newFrags =
            weightDown reso frags winner 

hasDirectOrIndirectDeadline :: [TaskFrag] -> TaskFrag -> Bool
hasDirectOrIndirectDeadline frags f =
    Prelude.any (hasDeadline) (successor frags f)

hasDeadline :: TaskFrag -> Bool
hasDeadline f =
    fDeadline f /= Nothing

successor :: [TaskFrag] -> TaskFrag -> [TaskFrag]
successor frags f =
    successor' frags f []

successor' :: [TaskFrag] -> TaskFrag -> [TaskFrag] -> [TaskFrag]
successor' frags f out 
    | Prelude.null neighborhoods =
        out
    | otherwise =
        Prelude.concat $ map (\frag -> successor' frags frag (Prelude.concat [neighborhoods,out])) neighborhoods 
        where
            neighborhoods =
                filter (\frag -> snd (fPair frag) == fst (fPair f)) frags 

predecessor :: [TaskFrag] -> TaskFrag -> [TaskFrag]
predecessor frags f =
    predecessor' frags f []

predecessor' :: [TaskFrag] -> TaskFrag -> [TaskFrag] -> [TaskFrag]
predecessor' frags f out 
    | Prelude.null neighborhoods =
        out
    | otherwise =
        Prelude.concat $ map (\frag -> predecessor' frags frag (Prelude.concat [neighborhoods,out])) neighborhoods 
        where
            neighborhoods =
                filter (\frag -> fst (fPair frag) == snd (fPair f)) frags 

hasWeight :: TaskFrag -> Bool
hasWeight f =
    case fWeight f of
        Just w ->
            w > 0
        _ ->
            False

isExecutable :: Millis -> [TaskFrag] -> TaskFrag -> Bool
isExecutable cursor frags f =
    (case fStartable f of
        Just s ->
            s <= cursor
        _ ->
            True
    ) && Prelude.all (\frag -> not $ hasWeight frag) (predecessor frags f)

urgency :: Millis -> [TaskFrag] -> TaskFrag -> Maybe Millis
urgency cursor frags f =
    let paths = routeFindF frags f
    in  Prelude.maximum $ map (urgencyByPath cursor) paths


urgencyByPath :: Millis -> [TaskFrag] -> Maybe Millis
urgencyByPath cursor path
    | Prelude.null $ filter (hasDeadline) path =
        Nothing
    | otherwise =
        case grandDeadline path of
            Just gd ->
                Just $ cursor + (totalWeight path) - gd
            _ ->
                Nothing
    where
        grandDeadline =
            Prelude.maximum . (map fDeadline) . (filter hasDeadline)

totalWeight :: [TaskFrag] -> Millis
totalWeight path =
    sum $ map   (\frag -> case fWeight frag of 
                    Nothing ->
                        0
                    Just w ->
                        w
                ) path

routeFindF :: [TaskFrag] -> TaskFrag -> [[TaskFrag]]
routeFindF frags f =
    routeFindF' f (nextFrags frags f) [] frags [] []

routeFindF' :: TaskFrag -> [TaskFrag] -> [TaskFrag] -> [TaskFrag] -> [TaskFrag] -> [[TaskFrag]] -> [[TaskFrag]]
routeFindF' _ [] [] _ stack out =
    (reverse stack) : out
routeFindF' current (x:xs) remain chart stack out =
    routeFindF' x (nextFrags chart x) (current:remain) (filter (/= x) chart) (x:stack) out
routeFindF' current [] (r:remain) chart stack out =
    routeFindF' r (nextFrags chart r) remain chart (Prelude.dropWhile (/= r) stack) (stack:out)

nextFrags :: [TaskFrag] -> TaskFrag -> [TaskFrag]
nextFrags chart x =
    filter (\frag -> snd (fPair frag) == fst (fPair x)) chart

assignTime :: [MillisDuration] -> Millis -> Millis -> [MillisDuration]
assignTime pattern reso cursor =
    assignTime' pattern reso cursor reso []

assignTime' :: [MillisDuration] -> Millis -> Millis -> Millis -> [MillisDuration] -> [MillisDuration]
assignTime' [] _ _ _ _ = []
assignTime' ((l,r):(l',r'):pattern) reso cursor rest out
    | cursor < l =
        assignTime' ((l,r):(l',r'):pattern) reso l rest out
    | l <= cursor && cursor < r && r < cursor + rest =
        assignTime' ((l',r'):pattern) reso l' (cursor + rest - r) ((cursor,r):out)
    |  l <= cursor && cursor < r =
        (cursor, cursor + reso) : out
    | otherwise =
        assignTime' ((l',r'):pattern) reso cursor rest out

weightDown :: Millis -> [TaskFrag] -> TaskFrag -> [TaskFrag]
weightDown reso frags winner =
    map (\frag -> if frag == winner then 
        let TaskFrag p ms md mw = winner
        in  TaskFrag p ms md ((-) <$> mw <*> Just reso)
        else
            frag
        ) frags
