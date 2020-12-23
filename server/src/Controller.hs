{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}



module Controller where



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

import Data.Time.Clock (nominalDay)
import Data.List (sort, maximumBy, intersect, (\\), sortOn, union, last, delete, find, nub)
import Data.Text.Format (fixed)
import Data.List.Unique (allUnique, sortUniq, isUnique)
import Data.Maybe (isNothing, fromJust)

import Debug.Trace



-- type alias



anonymousUser = toSqlKey 1 :: UserId

-- data ZoneName =
--     Name String
--     | Offset Int

type TimeZoneHour = Int
type Minutes = Int
type Millis = Int



-- DATA DECLARATION



data ElmSchedule = ElmSchedule
    { elmScheduleBegin  :: Millis
    , elmScheduleEnd    :: Millis
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmSchedule)

data ElmUser = ElmUser
    { elmUserId         :: Int
    , elmUserName       :: Text
    , elmUserDefaultDot :: Text
    , elmUserZoneName   :: Maybe Text
    , elmUserZoneOffset :: Maybe Minutes
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmUser)

data ElmTask = ElmTask
    { elmTaskId         :: Int
    , elmTaskIsDone     :: Bool
    , elmTaskIsStarred  :: Bool
    , elmTaskTitle      :: Maybe Text
    , elmTaskLink       :: Maybe Text
    , elmTaskStartable  :: Maybe Millis
    , elmTaskDeadline   :: Maybe Millis
    , elmTaskWeight     :: Maybe Double
    , elmTaskAssign     :: Text
    , elmTaskSchedule   :: [ElmSchedule]
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmTask)

data ElmMessage = ElmMessage
    { elmMessageCode  :: Int
    , elmMessageBody  :: Text
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmMessage)

data ElmSubModel = ElmSubModel
    { elmSubModelUser       :: ElmUser
    , elmSubModelTasks      :: [ElmTask]
    , elmSubModelInputText  :: Maybe Text
    , elmSubModelMessage    :: ElmMessage
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmSubModel)

data TextPost = TextPost
    { textPostUser      :: ElmUser
    , textPostContent   :: Text
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''TextPost)

data ElmUserTasks = ElmUserTasks
    { elmUserTasksUser :: ElmUser
    , elmUserTasksTasks  :: [Int]
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmUserTasks)

data ElmUserTask = ElmUserTask
    { elmUserTaskUser :: ElmUser
    , elmUserTaskTask :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmUserTask)

data SlashCmd
    = SlashSel Text
    | SlashDot Text
    | SlashAllow Text Text Text
    | SlashBan Text Text Text

data Condition
    = SelTitle Text
    | SelNotTitle Text 
    | SelStartableL Int Int Int Int Int
    | SelStartableR Int Int Int Int Int
    | SelDeadlineL Int Int Int Int Int
    | SelDeadlineR Int Int Int Int Int
    | SelWeightL Double
    | SelWeightR Double
    | SelAssign Text
    | SelNotAssign Text
    | SelArchived
    | SelUndone
    | SelStarred
    | SelTrunks
    | SelBuds
    | SelRelationL Int 
    | SelRelationR Int
  


-- API DEFINITION



type API =  "dev"   :> "model"  :> Capture "user"  Int          :> Get  '[JSON] ElmSubModel
    :<|>    "tasks" :> "init"   :> ReqBody '[JSON] ElmUser      :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "post"   :> ReqBody '[JSON] TextPost     :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "done"   :> ReqBody '[JSON] ElmUserTasks :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "undone" :> ReqBody '[JSON] ElmUserTasks :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "star"   :> ReqBody '[JSON] ElmUserTask  :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "focus"  :> ReqBody '[JSON] ElmUserTask  :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "home"   :> ReqBody '[JSON] ElmUser      :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "clone"  :> ReqBody '[JSON] ElmUserTasks :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "arch"   :> ReqBody '[JSON] ElmUser      :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "trunks" :> ReqBody '[JSON] ElmUser      :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "buds"   :> ReqBody '[JSON] ElmUser      :> Post '[JSON] ElmSubModel

startApp :: IO ()
startApp = run 3333 app

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
    :<|> undoneTasksReload
    :<|> switchStar
    :<|> focusTask
    :<|> goHome
    :<|> cloneTasks
    :<|> showArchives
    :<|> showTrunks
    :<|> showBuds
    where
        devSubModel :: Int -> Handler ElmSubModel
        devSubModel = liftIO . devSubModel'
        initialize :: ElmUser -> Handler ElmSubModel
        initialize = liftIO . initialize'
        textPostReload :: TextPost -> Handler ElmSubModel
        textPostReload = liftIO . textPostReload'
        doneTasksReload :: ElmUserTasks -> Handler ElmSubModel
        doneTasksReload = liftIO . doneTasksReload'
        undoneTasksReload :: ElmUserTasks -> Handler ElmSubModel
        undoneTasksReload = liftIO . undoneTasksReload'
        switchStar :: ElmUserTask -> Handler ElmSubModel
        switchStar = liftIO . switchStar'
        focusTask :: ElmUserTask -> Handler ElmSubModel
        focusTask = liftIO . focusTask'
        goHome :: ElmUser -> Handler ElmSubModel
        goHome = liftIO . goHome'
        cloneTasks :: ElmUserTasks -> Handler ElmSubModel
        cloneTasks = liftIO . cloneTasks'
        showArchives :: ElmUser -> Handler ElmSubModel
        showArchives = liftIO . showArchives'
        showTrunks :: ElmUser -> Handler ElmSubModel
        showTrunks = liftIO . showTrunks'
        showBuds :: ElmUser -> Handler ElmSubModel
        showBuds = liftIO . showBuds'

devSubModel' :: Int -> IO ElmSubModel
devSubModel' uid' = do
    pool <- pgPool
    let uid = keyFromId uid' :: UserId
    user <- Prelude.head <$> getUser pool uid
    durations <- getDurationsByUser pool uid
    let elmUser = toElmUser user durations
    elmTasks <- buildElmTasksByUser uid
    let okMsg = ElmMessage 200 $ intercalate " " ["Hello", elmUserName elmUser]
    return $ ElmSubModel elmUser elmTasks Nothing okMsg

initialize' :: ElmUser -> IO ElmSubModel
initialize' elmUser = do
    let uid' = elmUserId elmUser
    devSubModel' uid'

textPostReload' :: TextPost -> IO ElmSubModel
textPostReload' (TextPost elmUser text) = do
    if isCommand text then commandPostReload (TextPost elmUser text) else do
        pool <- pgPool
        let uid = keyFromId $ elmUserId elmUser :: UserId
        user <- Prelude.head <$> (map entityVal) <$> getUser pool uid
        let taskMaybeIdAssigns = tasksFromText user text
        let edges = edgesFromText text
        fault <- faultPost uid taskMaybeIdAssigns edges
        case fault of
            Just errMsg ->
                return $ ElmSubModel elmUser [] Nothing errMsg
            Nothing -> do
                prediction <- insertOrUpdate uid taskMaybeIdAssigns edges
                case prediction of
                    Left errMsg' ->
                        return $ ElmSubModel elmUser [] Nothing errMsg'
                    Right (inserts, updates) -> do
                        edges' <- maybeUpdate updates edges
                        edges'' <- insertTasks inserts edges'
                        insPaths pool $ map pathFromEdge edges''
                        re <- reschedule uid
                        case re of
                            Left errMsg'' ->
                                return $ ElmSubModel elmUser [] Nothing errMsg''
                            _ -> do
                                elmTasks <- buildElmTasksByUser uid
                                let ElmMessage _ ok1 = buildOkMsg inserts " tasks registered."
                                let ElmMessage _ ok2 = buildOkMsg updates " tasks updated."
                                let okMsg = ElmMessage 200 (intercalate " " [ok1, ok2])
                                return $ ElmSubModel elmUser elmTasks Nothing okMsg

insertOrUpdate :: UserId -> [(Task, Maybe TaskId, Maybe AssignName)] -> [Edge] -> IO (Either ElmMessage ([(Int, Task)], [(Int, TaskId, Task)]))
insertOrUpdate uid xs es = 
    if  allUnique $ filter ((/=) Nothing) $ map (\(_,mid,_) -> mid) xs then
        insertOrUpdate' uid (zip [0..] xs) es ([], [])
    else
        return $ Left (ElmMessage 400 "Duplicate use of #.")

insertOrUpdate' :: UserId -> [(Int, (Task, Maybe TaskId, Maybe AssignName))] -> [Edge] -> ([(Int, Task)], [(Int, TaskId, Task)]) -> IO (Either ElmMessage ([(Int, Task)], [(Int, TaskId, Task)]))
insertOrUpdate' uid [] _ out =
    return $ Right out
insertOrUpdate' uid ((i,(t,mid,man)):xs) ref (ins,upd) =
    case mid of
        Nothing -> do
            task <- buildTask uid t man
            insertOrUpdate' uid xs ref (((i,task):ins),upd)
        Just tid -> do
            pool <- pgPool
            get  <- getMeByTask pool tid
            case get of
                [] -> 
                    return $ Left (ElmMessage 400 $ intercalate " " ["Task ID", pack $ show $ idFromKey tid, "does not exist."])
                _ -> do
                    isExtT <- isExistingTrunk tid
                    if  isTrunk ref i || (isBud ref i && isExtT) then do
                            task <- buildTask uid t man
                            insertOrUpdate' uid xs ref (ins,((i,tid,task):upd))
                    else
                        return $ Left (ElmMessage 400 $ intercalate "" ["Invalid use of #", pack $ show $ idFromKey tid, "."])

buildTask :: UserId -> Task -> Maybe AssignName -> IO Task
buildTask _ t (Just an) = do
    pool <- pgPool
    assign <- entityKey <$> Prelude.head <$> getUserByName pool an
    let Task d s ml ms md mw mt _ = t
    return $ Task d s ml ms md mw mt assign
buildTask uid t _ = do
    let Task d s ml ms md mw mt _ = t
    return $ Task d s ml ms md mw mt uid

isExistingTrunk :: TaskId -> IO (Bool)
isExistingTrunk tid = do
    pool <- pgPool
    allTrunks <- getTrunks pool
    return $ tid `elem` (map entityKey allTrunks)

maybeUpdate :: [(Int, TaskId, Task)] -> [Edge] -> IO [Edge]
maybeUpdate [] es = 
    return es
maybeUpdate ((i,tid,t):xs) es = do
    pool <- pgPool
    maybeUpdate' pool tid t
    let es' = alterEdge i tid es
    maybeUpdate xs es'

maybeUpdate' :: ConnectionPool -> TaskId -> Task -> IO ()
maybeUpdate' pool tid (Task d s ml ms md mw mt u)
    | d /= False    = do
        setTaskDone pool tid
        maybeUpdate' pool tid (Task False s ml ms md mw mt u)
    | s /= False    = do
        setTaskStarred pool tid
        maybeUpdate' pool tid (Task d False ml ms md mw mt u)
    | ml /= Nothing = do
        let (Just l) = ml
        setTaskLink pool tid l
        maybeUpdate' pool tid (Task d s Nothing ms md mw mt u)
    | ms /= Nothing = do
        let (Just ss) = ms
        setTaskStartable pool tid ss
        maybeUpdate' pool tid (Task d s ml Nothing md mw mt u)
    | md /= Nothing = do
        let (Just dd) = md
        setTaskDeadline pool tid dd
        maybeUpdate' pool tid (Task d s ml ms Nothing mw mt u)
    | mw /= Nothing = do
        let (Just w) = mw
        setTaskWeight pool tid w
        maybeUpdate' pool tid (Task d s ml ms md Nothing mt u)
    | mt /= Nothing = do
        let (Just tt) = mt
        setTaskTitle pool tid tt
        maybeUpdate' pool tid (Task d s ml ms md mw Nothing u)
    | u /= anonymousUser = do
        setTaskAssign pool tid u
        maybeUpdate' pool tid (Task d s ml ms md mw mt anonymousUser)
    | otherwise = 
        return ()

insertTasks :: [(Int, Task)] -> [Edge] -> IO [Edge]
insertTasks [] es =
    return es
insertTasks ((i,t):xs) es = do
    pool <- pgPool
    tid <- insTask pool t
    let es' = alterEdge i tid es
    insertTasks xs es'

faultPost :: UserId -> [(Task, Maybe TaskId, Maybe AssignName)] -> [Edge] -> IO (Maybe ElmMessage)
faultPost uid taskMaybeIdAssigns es = do
    faultPerms <- faultPostPermission uid taskMaybeIdAssigns
    faultLoop <- faultPostLoop es
    let faultList = [ faultPerms
                    , faultLoop
                    ]
    if Prelude.all (== Nothing) faultList then
        return Nothing
    else do
        let mErrMsg = Prelude.head $ filter (/= Nothing) faultList
        return mErrMsg

faultPostPermission :: UserId -> [(Task, Maybe TaskId, Maybe AssignName)] -> IO (Maybe ElmMessage)
faultPostPermission _ [] =
    return Nothing
faultPostPermission sbj ((t,mid,man):xs) = do
    pool <- pgPool
    faultId <- case mid of
        Just tid -> do
            es <- getMeByTask pool tid
            case es of
                [] ->
                    return $ Just (ElmMessage 400 $ intercalate " " ["Task ID", pack $ show $ idFromKey tid, "does not exist."])
                (e:_) -> do
                    let obj = taskAssign $ entityVal e
                    faultPermission Edit sbj obj
        _ ->
            return Nothing
    faultAssign <- case man of
        Just an -> do
            es <- getUserByName pool an
            case es of
                [] ->
                    return $ Just (ElmMessage 400 $ intercalate " " ["User", an, "does not exist."])
                (e:_) -> do
                    let obj = entityKey e
                    faultPermission Edit sbj obj
        _ ->
            return Nothing
    let faultList = [faultId, faultAssign]
    if Prelude.all (== Nothing) faultList then
        faultPostPermission sbj xs
    else do
        let mErrMsg = Prelude.head $ filter (/= Nothing) faultList
        return mErrMsg

data Mode = View
          | Edit

faultPermission :: Mode -> UserId -> UserId -> IO (Maybe ElmMessage)
faultPermission mode sbj obj = do
    pool <- pgPool
    perm <- case mode of
        View ->
            getViewPerm pool sbj obj
        Edit ->
            getEditPerm pool sbj obj
    if sbj == obj || (not $ Prelude.null perm) then
        return Nothing
    else do
        sbjN <- userName <$> entityVal <$> Prelude.head <$> getUser pool sbj
        objN <- userName <$> entityVal <$> Prelude.head <$> getUser pool obj
        let modeStr = case mode of
                View ->
                    "view"
                Edit ->
                    "edit"
        return $ Just (ElmMessage 400 $ intercalate " " [sbjN, "cannot", modeStr, objN, "'s tasks."])

faultPermissionByTasks :: Mode -> UserId -> [TaskId] -> IO (Maybe ElmMessage)
faultPermissionByTasks mode sbj [] = 
    return Nothing
faultPermissionByTasks mode sbj (tid:tids) = do
    pool <- pgPool
    obj <- taskAssign <$> entityVal <$> Prelude.head <$> getMeByTask pool tid
    fault <- faultPermission mode sbj obj
    case fault of
        Just errMsg ->
            return $ Just errMsg
        _ ->
            faultPermissionByTasks mode sbj tids

faultPostLoop :: [Edge] -> IO (Maybe ElmMessage)
faultPostLoop es = do
    if hasLoop es then
        return $ Just (ElmMessage 400 "Posts with loops are not accepted.")
    else
        return $ Nothing

doneTasksReload' :: ElmUserTasks -> IO ElmSubModel
doneTasksReload' (ElmUserTasks elmUser tids') = do
    pool <- pgPool
    let uid = keyFromId $ elmUserId elmUser :: UserId
    let tids = map (\x -> keyFromId x :: TaskId) tids'
    fault <- faultPermissionByTasks Edit uid tids
    case fault of
        Just errMsg ->
            return $ ElmSubModel elmUser [] Nothing errMsg
        Nothing -> do
            usAndPreds <- usAndPredecessors tids
            all <- map entityKey <$> getAllTasksByUser pool uid
            undone <- map entityKey <$> getUndoneTasks pool
            let targets = usAndPreds `intersect` all `intersect` undone
            sequence_ $ map (setTaskDone pool) targets
            re <- reschedule uid
            case re of
                Left errMsg' ->
                    return $ ElmSubModel elmUser [] Nothing errMsg' 
                _ -> do
                    elmTasks <- buildElmTasksByUser uid
                    let okMsg = buildOkMsg targets " tasks archived."
                    return $ ElmSubModel elmUser elmTasks Nothing okMsg

usAndPredecessors :: [TaskId] -> IO [TaskId]
usAndPredecessors tids = do
    pool <- pgPool
    paths <- map entityVal <$> getAllPaths pool
    let edges = map edgeFromPath paths
    let tids' = map idFromKey tids
    let preds' = sortUniq $ Prelude.concatMap (predecessors' edges) tids'
    let preds = map (\tid' -> keyFromId tid' :: TaskId) preds'
    return $ tids `union` preds

undoneTasksReload' :: ElmUserTasks -> IO ElmSubModel
undoneTasksReload' (ElmUserTasks elmUser tids') = do
    pool <- pgPool
    let uid = keyFromId $ elmUserId elmUser :: UserId
    let tids = map (\x -> keyFromId x :: TaskId) tids'
    fault <- faultPermissionByTasks Edit uid tids
    case fault of
        Just errMsg ->
            return $ ElmSubModel elmUser [] Nothing errMsg
        Nothing -> do
            usAndSuccs <- usAndSuccessors tids
            all <- map entityKey <$> getAllTasksByUser pool uid
            done <- map entityKey <$> getDoneTasks pool
            let targets = usAndSuccs `intersect` all `intersect` done
            sequence_ $ map (setTaskUndone pool) targets
            re <- reschedule uid
            case re of
                Left errMsg' ->
                    return $ ElmSubModel elmUser [] Nothing errMsg' 
                _ -> do
                    elmTasks <- buildElmTasksByUser uid
                    let okMsg = buildOkMsg targets " tasks undone."
                    return $ ElmSubModel elmUser elmTasks Nothing okMsg

usAndSuccessors :: [TaskId] -> IO [TaskId]
usAndSuccessors tids = do
    pool <- pgPool
    paths <- map entityVal <$> getAllPaths pool
    let edges = map edgeFromPath paths
    let tids' = map idFromKey tids
    let succs' = sortUniq $ Prelude.concatMap (successors' edges) tids'
    let succs = map (\tid' -> keyFromId tid' :: TaskId) succs'
    return $ tids `union` succs

switchStar' :: ElmUserTask -> IO ElmSubModel
switchStar' (ElmUserTask elmUser tid') = do
    pool <- pgPool
    let uid = keyFromId $ elmUserId elmUser :: UserId
    let tid = keyFromId tid' :: TaskId
    fault <- faultPermissionByTasks Edit uid [tid]
    case fault of
        Just errMsg ->
            return $ ElmSubModel elmUser [] Nothing errMsg
        _ -> do
            setStarSwitched pool tid
            return $ ElmSubModel elmUser [] Nothing (ElmMessage 200 "")

focusTask' :: ElmUserTask -> IO ElmSubModel
focusTask' (ElmUserTask elmUser tid') = do
    pool    <- pgPool
    let uid = keyFromId $ elmUserId elmUser :: UserId
    let tid = keyFromId tid' :: TaskId
    before  <- map entityKey <$> getBeforeMeByTask pool tid
    me      <- map entityKey <$> getMeByTask pool tid
    after   <- map entityKey <$> getAfterMeByTask pool tid
    all     <- map entityKey <$> getAllTasksByUser pool uid
    let targets = (Prelude.concat [before, me, after]) `intersect` all
    elmTasks <- sequence (map buildElmTaskByTask targets) 
    return $ ElmSubModel elmUser elmTasks Nothing (ElmMessage 200 "")

goHome' :: ElmUser -> IO ElmSubModel
goHome' elmUser = do
    pool <- pgPool
    let uid = keyFromId $ elmUserId elmUser :: UserId
    elmTasks <- buildElmTasksByUser uid
    let okMsg = buildOkMsg elmTasks " tasks here."
    return $ ElmSubModel elmUser elmTasks Nothing okMsg

cloneTasks' :: ElmUserTasks -> IO ElmSubModel
cloneTasks' (ElmUserTasks elmUser tids') = do
    pool <- pgPool
    let uid = keyFromId $ elmUserId elmUser :: UserId
    let tids = map (\x -> keyFromId x :: TaskId) tids'
    user <- entityVal <$> Prelude.head <$> getUser pool uid
    tasks' <- (map (\(e,v) -> (entityVal e, entityKey e, Q.unValue v))) <$> getTaskAssigns tids
    let tasks = map (localize' user) tasks'
    let nodes = map nodeFromTask tasks
    paths <- map entityVal <$> getPathsByTasks pool tids
    let edges = map edgeFromPath paths
    let text = textFromGraph (nodes, edges)
    let okMsg = buildOkMsg tasks " tasks cloned."
    return $ ElmSubModel elmUser [] (Just text) okMsg

getTaskAssigns :: [TaskId] -> IO [(Entity Task, Q.Value Text)]
getTaskAssigns tids = do
    pool <- pgPool
    Prelude.concat <$> sequence (map (getTaskAssignByTask pool) tids)

showArchives' :: ElmUser -> IO ElmSubModel
showArchives' elmUser = do
    pool <- pgPool
    let uid = keyFromId $ elmUserId elmUser :: UserId
    own  <- map entityKey <$> getOwnTasksByUser pool uid
    done <- map entityKey <$> getDoneTasks pool
    let targets = done `intersect` own
    elmTasks <- sequence (map buildElmTaskByTask targets) 
    let okMsg = buildOkMsg elmTasks " archives here."
    return $ ElmSubModel elmUser elmTasks Nothing okMsg

showTrunks' :: ElmUser -> IO ElmSubModel
showTrunks' elmUser = do
    pool <- pgPool
    let uid = keyFromId $ elmUserId elmUser :: UserId
    own   <- map entityKey <$> getOwnTasksByUser pool uid
    undone <- map entityKey <$> getUndoneTasks pool
    trunk <- map entityKey <$> getTrunks pool
    let targets = own `intersect` undone `intersect` trunk
    elmTasks <- sequence (map buildElmTaskByTask targets) 
    let okMsg = buildOkMsg elmTasks " trunks here."
    return $ ElmSubModel elmUser elmTasks Nothing okMsg

showBuds' :: ElmUser -> IO ElmSubModel
showBuds' elmUser = do
    pool <- pgPool
    let uid = keyFromId $ elmUserId elmUser :: UserId
    own  <- map entityKey <$> getOwnTasksByUser pool uid
    undone <- map entityKey <$> getUndoneTasks pool
    bud  <- map entityKey <$> getBuds pool
    let targets = own `intersect` undone `intersect` bud
    elmTasks <- sequence (map buildElmTaskByTask targets) 
    let okMsg = buildOkMsg elmTasks " buds here."
    return $ ElmSubModel elmUser elmTasks Nothing okMsg

reschedule :: UserId -> IO (Either ElmMessage ())
reschedule uid = do
    pool    <- pgPool
    user    <- entityVal <$> Prelude.head <$> getUser pool uid
    now     <- getCurrentTime
    durs    <- map entityVal <$> getDurationsByUser pool uid
    own     <- getOwnTasksByUser pool uid
    undone  <- getUndoneTasks pool
    let targets = own `intersect` undone
    path    <- map entityVal <$> getPathsByTasks pool (map entityKey targets)
    let sch =
            if userIsLazy user then Left (ElmMessage 400 "Now implementing")  --TODO scheduleBackward
            else scheduleForward user now durs targets path
    case sch of
        Left errMsg ->
            return $ Left errMsg
        Right schedules -> do
            resetSchedules uid schedules
            return $ Right ()

buildElmTaskByTask :: TaskId -> IO ElmTask
buildElmTaskByTask tid = do
    pool <- pgPool
    taskAssign <- Prelude.head <$> getTaskAssignByTask pool tid
    schedules <- map entityVal <$> getSchedulesByTask pool tid
    return $ toElmTask taskAssign schedules

buildElmTasksByUser :: UserId -> IO [ElmTask] 
buildElmTasksByUser uid = do
    pool <- pgPool
    own    <- map entityKey <$> getOwnTasksByUser pool uid
    undone <- map entityKey <$> getUndoneTasks pool
    let targets = own `intersect` undone
    sequence (map buildElmTaskByTask targets) 

resetSchedules :: UserId -> [Schedule] -> IO ()
resetSchedules uid schedules = do
    pool <- pgPool
    delSchedulesByUser pool uid
    insSchedules pool schedules

commandPostReload :: TextPost -> IO ElmSubModel
commandPostReload (TextPost elmUser cmd) = do
    case parseOnly aSlashCmd cmd of
        Right (SlashSel conditions) ->
            slashSel elmUser conditions
        Right (SlashDot unit) ->
            slashDot elmUser unit
        Right (SlashAllow sbj act obj) ->
            slashAllow elmUser sbj act obj
        Right (SlashBan sbj act obj) ->
            slashBan elmUser sbj act obj
        _ ->
            return $ ElmSubModel elmUser [] Nothing (ElmMessage 400 "Invalid command.")

slashSel :: ElmUser -> Text -> IO ElmSubModel
slashSel elmUser conditions = do
    pool <- pgPool
    let uid = keyFromId $ elmUserId elmUser :: UserId
    let cons = filter (/= "") (splitOn "-" conditions)
    tids' <- map entityKey <$> getAllTasksByUser pool uid
    tids <- slashSel' uid cons tids'
    elmTasks <- sequence (map buildElmTaskByTask tids)
    let okMsg = buildOkMsg elmTasks " tasks selected."
    return $ ElmSubModel elmUser elmTasks Nothing okMsg

slashSel' :: UserId -> [Text] -> [TaskId] -> IO [TaskId]
slashSel' _ [] tids =
    return tids
slashSel' uid (con:cons) tids = do
    pool <- pgPool
    case parseOnly aCondition con of 
        Right (SelTitle text) -> do
            let words = filter (/= "") (splitOn " " text)
            sels <- sequence $ map (selTitle pool) words
            let sel = Prelude.foldl1 union $ map (map entityKey) sels
            slashSel' uid cons (tids `intersect` sel)

        Right (SelNotTitle text) -> do
            let words = filter (/= "") (splitOn " " text)
            sels <- sequence $ map (selNotTitle pool) words
            let sel = Prelude.foldl1 union $ map (map entityKey) sels
            slashSel' uid cons (tids `intersect` sel)

        Right (SelStartableL y m d hou min) -> do
            let date = fromGregorian (fromIntegral y) m d
            let clock = secondsToDiffTime . fromIntegral $ 60 * (min + 60 * hou)
            sel <- selStartableL pool (UTCTime date clock)
            slashSel' uid cons (tids `intersect` (map entityKey sel))

        Right (SelStartableR y m d hou min) -> do
            let date = fromGregorian (fromIntegral y) m d
            let clock = secondsToDiffTime . fromIntegral $ 60 * (min + 60 * hou)
            sel <- selStartableR pool (UTCTime date clock)
            slashSel' uid cons (tids `intersect` (map entityKey sel))

        Right (SelDeadlineL y m d hou min) -> do
            let date = fromGregorian (fromIntegral y) m d
            let clock = secondsToDiffTime . fromIntegral $ 60 * (min + 60 * hou)
            sel <- selDeadlineL pool (UTCTime date clock)
            slashSel' uid cons (tids `intersect` (map entityKey sel))

        Right (SelDeadlineR y m d hou min) -> do
            let date = fromGregorian (fromIntegral y) m d
            let clock = secondsToDiffTime . fromIntegral $ 60 * (min + 60 * hou)
            sel <- selDeadlineR pool (UTCTime date clock)
            slashSel' uid cons (tids `intersect` (map entityKey sel))

        Right (SelWeightL w) -> do
            sel <- selWeightL pool w
            slashSel' uid cons (tids `intersect` (map entityKey sel))

        Right (SelWeightR w) -> do
            sel <- selWeightR pool w
            slashSel' uid cons (tids `intersect` (map entityKey sel))

        Right (SelAssign text) -> do
            let names = filter (/= "") (splitOn " " text)
            sels <- sequence $ map (selAssign pool) names
            let sel = Prelude.foldl1 union $ map (map entityKey) sels
            slashSel' uid cons (tids `intersect` sel)

        Right (SelNotAssign text) -> do
            let names = filter (/= "") (splitOn " " text)
            sels <- sequence $ map (selNotAssign pool) names
            let sel = Prelude.foldl1 union $ map (map entityKey) sels
            slashSel' uid cons (tids `intersect` sel)

        Right SelArchived -> do
            sel <- map entityKey <$> getDoneTasks pool
            slashSel' uid cons (tids `intersect` sel)

        Right SelUndone -> do
            sel <- map entityKey <$> getUndoneTasks pool
            slashSel' uid cons (tids `intersect` sel)

        Right SelStarred -> do
            sel <- map entityKey <$> getStarredTasks pool
            slashSel' uid cons (tids `intersect` sel)

        Right SelTrunks -> do
            sel <- map entityKey <$> getTrunks pool
            slashSel' uid cons (tids `intersect` sel)

        Right SelBuds -> do
            sel <- map entityKey <$> getBuds pool
            slashSel' uid cons (tids `intersect` sel)

        Right (SelRelationL tid') -> do
            succs <- usAndSuccessors [keyFromId tid' :: TaskId]
            slashSel' uid cons (tids `intersect` succs)

        Right (SelRelationR tid') -> do
            preds <- usAndPredecessors [keyFromId tid' :: TaskId]
            slashSel' uid cons (tids `intersect` preds)

        _ ->
            slashSel' uid cons tids

slashDot :: ElmUser -> Text -> IO ElmSubModel
slashDot elmUser unit
    | unit `elem` ["Y", "Q", "M", "W", "D", "6h", "h", "12m", "m", "s"] = do
        pool <- pgPool
        let uid = keyFromId $ elmUserId elmUser :: UserId
        setDot pool uid unit
        let elmUser' = (\(ElmUser i n _ mzn mzo) -> ElmUser i n unit mzn mzo) elmUser
        return $ ElmSubModel elmUser' [] Nothing (ElmMessage 300 (Data.Text.concat ["Default time unit: ", unit]))
    | otherwise =
        return $ ElmSubModel elmUser [] Nothing (ElmMessage 400 (Data.Text.concat ["Invalid time unit: ", unit]))

slashAllow :: ElmUser -> Text -> Text -> Text -> IO ElmSubModel
slashAllow elmUser sbj act obj = do
    pool <- pgPool
    (existS, eSs) <- existsUser sbj
    (existO, eOs) <- existsUser obj
    if Prelude.and [existS, existO] then do
        let sKey = entityKey . Prelude.head $ eSs
        let oKey = entityKey . Prelude.head $ eOs
        case act of
            "view" -> do
                let perm = Permission sKey oKey True False
                ins <- insUnqPerm pool perm
                case ins of
                    Nothing -> do
                        setAllowView pool sKey oKey
                        return $ ElmSubModel elmUser [] Nothing (ElmMessage 300 (Data.Text.concat [sbj, " -> ", obj, " : view OK edit NG"]))
                    _ ->
                        return $ ElmSubModel elmUser [] Nothing (ElmMessage 300 (Data.Text.concat [sbj, " -> ", obj, " : view OK edit NG"]))
            "edit" -> do
                let perm = Permission sKey oKey True True
                ins <- insUnqPerm pool perm
                case ins of
                    Nothing -> do
                        setAllowEdit pool sKey oKey
                        return $ ElmSubModel elmUser [] Nothing (ElmMessage 300 (Data.Text.concat [sbj, " -> ", obj, " : view OK edit OK"]))
                    _ ->
                        return $ ElmSubModel elmUser [] Nothing (ElmMessage 300 (Data.Text.concat [sbj, " -> ", obj, " : view OK edit OK"]))
            a ->
                return $ ElmSubModel elmUser [] Nothing (ElmMessage 400 (Data.Text.concat ["Invalid action: ", a]))
    else
        return $ ElmSubModel elmUser [] Nothing (ElmMessage 400 "User does not exist.")

slashBan :: ElmUser -> Text -> Text -> Text -> IO ElmSubModel
slashBan elmUser sbj act obj = do
    pool <- pgPool
    (existS, eSs) <- existsUser sbj
    (existO, eOs) <- existsUser obj
    if Prelude.and [existS, existO] then do
        let sKey = entityKey . Prelude.head $ eSs
        let oKey = entityKey . Prelude.head $ eOs
        case act of
            "view" -> do
                let perm = Permission sKey oKey False False
                ins <- insUnqPerm pool perm
                case ins of
                    Nothing -> do
                        setBanView pool sKey oKey
                        return $ ElmSubModel elmUser [] Nothing (ElmMessage 300 (Data.Text.concat [sbj, " -> ", obj, " : view NG edit NG"]))
                    _ ->            
                        return $ ElmSubModel elmUser [] Nothing (ElmMessage 300 (Data.Text.concat [sbj, " -> ", obj, " : view NG edit NG"]))            
            "edit" -> do
                let perm = Permission sKey oKey True False
                ins <- insUnqPerm pool perm
                case ins of
                    Nothing -> do
                        setBanEdit pool sKey oKey
                        return $ ElmSubModel elmUser [] Nothing (ElmMessage 300 (Data.Text.concat [sbj, " -> ", obj, " : view OK edit NG"]))
                    _ ->            
                        return $ ElmSubModel elmUser [] Nothing (ElmMessage 300 (Data.Text.concat [sbj, " -> ", obj, " : view OK edit NG"]))
            a ->
                return $ ElmSubModel elmUser [] Nothing (ElmMessage 400 (Data.Text.concat ["Invalid action: ", a]))
    else
        return $ ElmSubModel elmUser [] Nothing (ElmMessage 400 "User does not exist.")

existsUser :: Text -> IO (Bool, [Entity User])
existsUser name = do
    pool <- pgPool
    users <- getUserByName pool name
    return $ (not . Prelude.null $ users, users)

initDb :: IO ()
initDb = do
    pool <- pgPool
    let developer = User "develop" True False 9 60 "Hou" Nothing
    insUser pool developer
    let developerDur = 
            [ Duration (TimeOfDay 8 30 0) (TimeOfDay 12 0 0) (toSqlKey 1 :: UserId)
            , Duration (TimeOfDay 13 00 0) (TimeOfDay 17 30 0) (toSqlKey 1 :: UserId)
            ]
    sequence_ $ map (insDur pool) developerDur



-- INTERNAL OPERATIONS



type Graph = ([Node], [Edge])
type Node  = (Int, [Attr])
type Edge  = (Int, Int)
data Attr  = 
      AttrTaskId    { attrTaskId    :: Int  }
    | IsDone
    | IsStarred
    | HeadLink      { headLink      :: Text }
    | Title         { title         :: Text }
    | StartableDate { startableYea  :: Int, startableMon    :: Int, startableDay    :: Int }
    | StartableTime { startableHou  :: Int, startableMin    :: Int }
    | Weight        { weight        :: Double }
    | DeadlineDate  { deadlineYea   :: Int, deadlineMon     :: Int, deadlineDay     :: Int }
    | DeadlineTime  { deadlineHou   :: Int, deadlineMin     :: Int }
    | AssignName    { assignName    :: Text }
    | TailLink      { tailLink      :: Text }
    | Link          { link          :: Text }
    deriving (Eq, Show, Ord)

tasksFromText :: User -> Text -> [(Task, Maybe TaskId, Maybe AssignName)]
tasksFromText u t =
    let
        triples = map taskFromNode (fst $ graphFromText t)
        tasks = map (universalize u) (map (\(task,_,_) -> task) triples)
    in
        Prelude.zipWith (\task (_,mid,man) -> (task,mid,man)) tasks triples

edgesFromText :: Text -> [Edge]
edgesFromText =
    snd . graphFromText

type AssignName = Text

taskFromNode :: Node -> (Task, Maybe TaskId, Maybe AssignName)
taskFromNode (_,as) =
    taskFromNode' as ((Task False False Nothing Nothing Nothing Nothing Nothing anonymousUser), Nothing, Nothing)

taskFromNode' :: [Attr] -> (Task, Maybe TaskId, Maybe AssignName) -> (Task, Maybe TaskId, Maybe AssignName)
taskFromNode' [] triple = 
    triple
taskFromNode' (a:as) ((Task d s ml ms md mw mt u), mid, man) =
    case a of
        AttrTaskId id ->
            taskFromNode' as ((Task d s ml ms md mw mt u), Just (keyFromId id :: TaskId), man)
        IsDone ->
            taskFromNode' as ((Task True s ml ms md mw mt u), mid, man)
        IsStarred ->
            taskFromNode' as ((Task d True ml ms md mw mt u), mid, man)
        Link l ->
            taskFromNode' as ((Task d s (Just l) ms md mw mt u), mid, man)
        StartableDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case ms of
                    Just (UTCTime od ot) ->
                        taskFromNode' as ((Task d s ml (Just (UTCTime nd ot)) md mw mt u), mid, man)
                    Nothing ->
                        taskFromNode' as ((Task d s ml (Just (UTCTime nd 0)) md mw mt u), mid, man)
        StartableTime hh mm ->
            let
                nt = secondsToDiffTime . fromIntegral $ 60 * (mm + 60 * hh)
            in
                case ms of
                    Just (UTCTime od ot) ->
                        taskFromNode' as ((Task d s ml (Just (UTCTime od nt)) md mw mt u), mid, man)
                    Nothing ->
                        let
                            nd = fromGregorian 3000 0 0
                        in
                            taskFromNode' as ((Task d s ml (Just (UTCTime nd nt)) md mw mt u), mid, man)
        DeadlineDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case md of
                    Just (UTCTime od ot) ->
                        taskFromNode' as ((Task d s ml ms (Just (UTCTime nd ot)) mw mt u), mid, man)
                    Nothing ->
                        taskFromNode' as ((Task d s ml ms (Just (UTCTime nd 0)) mw mt u), mid, man)
        DeadlineTime hh mm ->
            let
                nt = secondsToDiffTime . fromIntegral $ 60 * (mm + 60 * hh)
            in
                case md of
                    Just (UTCTime od ot) ->
                        taskFromNode' as ((Task d s ml ms (Just (UTCTime od nt)) mw mt u), mid, man)
                    Nothing ->
                        let
                            nd = fromGregorian 3000 0 0
                        in
                            taskFromNode' as ((Task d s ml ms (Just (UTCTime nd nt)) mw mt u), mid, man)
        Weight w ->
            taskFromNode' as ((Task d s ml ms md (Just w) mt u), mid, man)
        AssignName an ->
            taskFromNode' as ((Task d s ml ms md mw mt u), mid, Just an)
        Title t' ->
            case mt of
                Just t ->
                    taskFromNode' as ((Task d s ml ms md mw (Just $ Data.Text.intercalate " " [t', t]) u), mid, man)
                Nothing ->
                    taskFromNode' as ((Task d s ml ms md mw (Just t') u), mid, man)
        _ -> 
            taskFromNode' as ((Task d s ml ms md mw mt u), mid, man)

graphFromText :: Text -> Graph
graphFromText t = 
    let
        indents = map fst $ chopLines t
        wordss = map snd $ chopLines t
        nodes = enNode wordss
        edges = Prelude.concat [markUp indents, spanLink nodes]
    in
        (nodes, edges)

type Indent = Int
indent = "    " :: Text

chopLines :: Text -> [(Indent, [Text])]
chopLines =
    map indentAndWords . filter (/= "") . splitOn "\n"

indentAndWords :: Text -> (Indent, [Text])
indentAndWords =
    indentAndWords' 0

indentAndWords' :: Int -> Text -> (Indent, [Text])
indentAndWords' c t
    | Data.Text.take l t == indent =
        indentAndWords' (c + 1) (Data.Text.drop l t)
    | otherwise =
        (c, filter (/= "") $ splitOn " " t)
    where
        l = Data.Text.length indent

enNode :: [[Text]] -> [Node]
enNode wordss =
    zip [0..] (map enNode' wordss)

enNode' :: [Text] -> [Attr]
enNode' words =
    enNode'' words []

enNode'' :: [Text] -> [Attr] -> [Attr]
enNode'' [] out =
    out
enNode'' (w:ws) out =
    case parseOnly aAttr w of
        Left _ -> enNode'' ws out
        Right a -> enNode'' ws (a:out)

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
    <|> HeadLink      <$> takeTill (==']') <* char ']'
    <|> TailLink      <$  char '['  <*> takeText
    <|> AssignName    <$  char '@'  <*> takeText
    <|> Title         <$> takeText

spanLink :: [Node] -> [Edge]
spanLink nodes =
    spanLink' nodes nodes nodes []

spanLink' :: [Node] -> [Node] -> [Node] -> [Edge] -> [Edge]
spanLink' [] _ _ out = 
    out
spanLink' (x:xs) [] ref out = 
    spanLink' xs ref ref out
spanLink' (x:xs) (y:ys) ref out =
    spanLink' (x:xs) ys ref (Prelude.concat [spanLink'' x y y [], out])

spanLink'' :: Node -> Node -> Node -> [Edge] -> [Edge]
spanLink'' (_,[]) _ _ out =
    out
spanLink'' (i,(a:as)) (_,[]) ref out =
    spanLink'' (i,as) ref ref out
spanLink'' (i,(a:as)) (j,(b:bs)) ref out =
    case a of
        TailLink t ->
            case b of
                HeadLink h ->
                    if t == h then
                        spanLink'' (i,(a:as)) (j,bs) ref ((i,j):out)
                    else
                        spanLink'' (i,(a:as)) (j,bs) ref out
                _ ->
                    spanLink'' (i,(a:as)) (j,bs) ref out
        _ ->
            spanLink'' (i,as) (j,(b:bs)) ref out

markUp :: [Indent] -> [(Int, Int)]
markUp xs =
    markUp' ys (Prelude.drop 1 ys) ys []
    where
        ys = zip [0..] xs

markUp' ::  [(Int, Indent)] -> [(Int, Indent)] -> [(Int, Indent)] -> [(Int, Int)] -> [(Int, Int)]
markUp' [] _ _ out =
    out
markUp' ((i,d):xs) [] ref out =
    markUp' xs (Prelude.drop (i + 2) ref) ref out
markUp' ((i,d):xs) ((j,e):ys) ref out
    | d + 1 == e =
        markUp' ((i,d):xs) ys ref ((i,j):out)
    | d < e =
        markUp' ((i,d):xs) ys ref out
    | otherwise =
        markUp' xs (Prelude.drop (i + 2) ref) ref out

universalize :: User -> Task -> Task
universalize user = timeShift minus
    where
        minus = 60*60*(-1)*(fromIntegral $ userTimeZone user)

timeShift :: NominalDiffTime -> Task -> Task
timeShift diff (Task d s l ms md mw tt u) = 
    let
        msS = addUTCTime <$> Just diff <*> ms
        mdS = addUTCTime <$> Just diff <*> md
    in
        Task d s l msS mdS mw tt u

localize :: User -> Task -> Task
localize user = timeShift plus
    where
        plus = 60*60*(fromIntegral $ userTimeZone user)

localize' :: User -> (Task, TaskId, AssignName) -> (Task, TaskId, AssignName)
localize' u (t,i,a) =
    (localize u t, i, a)

edgeFromPath :: Path -> Edge
edgeFromPath p =
    (idFromKey $ pathTerminal p, idFromKey $ pathInitial p)

pathFromEdge :: Edge -> Path
pathFromEdge (t,i) =
    Path (keyFromId t :: TaskId) (keyFromId i :: TaskId)

nodeFromTask :: (Task, TaskId, AssignName) -> Node
nodeFromTask (task,key,an) =
    let
        attrs   =   [ AttrTaskId (idFromKey key)
                    , AssignName an
                    ]
        attrs'  =   nodeFromTask' task attrs
    in
        (idFromKey key, attrs')

nodeFromTask' :: Task -> [Attr] -> [Attr]
nodeFromTask' (Task d s ml ms md mw mt u) out
    | d =
        nodeFromTask' (Task False s ml ms md mw mt u) (IsDone : out)
    | s =
        nodeFromTask' (Task False False ml ms md mw mt u) (IsStarred : out)
    | ml /= Nothing =
        let
            Just l = ml
        in
            nodeFromTask' (Task False False Nothing ms md mw mt u) ((Link l) : out)
    | ms /= Nothing =
        let
            Just ss = ms
            UTCTime date clock = ss
            (yea, mon, day) = toGregorian date
            (hou, min, _) = toHMS clock
            sD = StartableDate (fromIntegral yea) mon day
            sT = StartableTime hou min
        in
            nodeFromTask' (Task False False Nothing Nothing md mw mt u) (sD : sT : out)
    | md /= Nothing =
        let
            Just dd = md
            UTCTime date clock = dd
            (yea, mon, day) = toGregorian date
            (hou, min, _) = toHMS clock
            dD = DeadlineDate (fromIntegral yea) mon day
            dT = DeadlineTime hou min
        in
            nodeFromTask' (Task False False Nothing Nothing Nothing mw mt u) (dD : dT : out)
    | mw /= Nothing =
        let
            Just w = mw
        in
            nodeFromTask' (Task False False Nothing Nothing Nothing Nothing mt u) ((Weight w) : out)
    | mt /= Nothing =
        let
            Just tt = mt
        in
            nodeFromTask' (Task False False Nothing Nothing Nothing Nothing Nothing u) ((Title tt) : out)
    | otherwise =
        out

toHMS :: DiffTime -> (Int, Int, Int)
toHMS d =
    let
        sec = fromIntegral $ (diffTimeToPicoseconds d) `div` (10^12) :: Int
        (h,rem) = ((sec `div` 3600), (sec `mod` 3600))
        (m, s) = ((rem `div` 60), (rem `mod` 60))
    in
        (h, m, s)

textFromGraph :: Graph -> Text
textFromGraph g =
    let
        (nodes, edges) = asideLink g
        indexedIndents = markDown (nodes, edges)
        indexedWordss = deNode nodes
    in
        trace (Prelude.concat [show nodes, show edges, show indexedIndents, show indexedWordss])
            $ bindLines indexedIndents indexedWordss

asideLink :: Graph -> ([Node], [Edge])
asideLink g =
    let
        routes = sortByUniqueness . routeFind $ g
        (regular, aside) = regularOrAside routes
        nodes = fst g
    in
        (addLink aside nodes, regular)

type Route = [Int]

routeFind :: Graph -> [Route]
routeFind g =
    Prelude.concatMap (routeFind' (snd g)) (buds g)

buds :: Graph -> [Int]
buds (ns, es) =
    filter (\x -> x `notElem` (map fst es)) (map fst ns)

trunks :: Graph -> [Int]
trunks (ns, es) =
    filter (\x -> x `notElem` (map snd es)) (map fst ns)

routeFind' :: [Edge] -> Int -> [Route]
routeFind' es bud =
    routeObserve bud (succ_ bud es) [] (map (\v -> (v, bud)) (succ_ bud es)) es [bud] []

routeObserve :: Int -> [Int] -> [Int] -> [Edge] -> [Edge] -> Route -> [Route] -> [Route]
routeObserve _ [] [] _ _ stack out =
    stack:out
routeObserve current (x:xs) remain cha chart stack out =
    routeExplore current (x:xs) remain (lightenSucc cha chart current x) chart stack out
routeObserve current [] (r:remain) cha chart stack out =
    routeExplore current [] (r:remain) cha chart stack out

routeExplore :: Int -> [Int] -> [Int] -> [Edge] -> [Edge] -> Route -> [Route] -> [Route]
routeExplore _ [] [] _ _ stack out =
    stack:out
routeExplore current (x:[]) remain cha chart stack out =
    routeObserve x (succ_ x cha) remain cha chart (x:stack) out
routeExplore current (x:_) remain cha chart stack out =
    routeObserve x (succ_ x cha) (current:remain) cha chart (x:stack) out
routeExplore current [] (r:remain) cha chart stack out =
    routeObserve r (succ_ r cha) remain cha chart (Prelude.dropWhile (/= r) stack) (stack:out)

lightenSucc :: [Edge] -> [Edge] -> Int -> Int -> [Edge]
lightenSucc cha chart current x =
    Prelude.concat [filter (/= (x, current)) cha, filter (\e -> snd e == x) chart]

sortByUniqueness :: [Route] -> [Route]
sortByUniqueness rs =
    sortByUniqueness' (rs \\ routesWithUniqueBud) routesWithUniqueBud
    where
        routesWithUniqueBud = filter (\r -> fromJust $ (Data.List.last r) `isUnique` (map Data.List.last rs)) rs

sortByUniqueness' :: [Route] -> [Route] -> [Route]
sortByUniqueness' [] out =
    out
sortByUniqueness' rs out =
    sortByUniqueness' (delete winner rs) (winner:out)
    where
        winner = Data.List.last $ sortOn (rUniqueness out) rs

rUniqueness :: [Route] -> Route -> Int
rUniqueness rs r =
    1 * Prelude.length uniqueV + (-1) * Prelude.length (r \\ uniqueV)
    where
        uniqueV = filter (\x -> x `notElem` Prelude.concatMap id rs) r

regularOrAside :: [Route] -> ([Edge], [Edge])
regularOrAside rs =
    regularOrAside' (Prelude.concatMap edgesFromRoute rs) ([],[])

regularOrAside' :: [Edge] -> ([Edge], [Edge]) -> ([Edge], [Edge])
regularOrAside' [] out =
    out
regularOrAside' (e:es) (regular, aside)
    | (e `notElem` (Prelude.concat [regular, aside])) && ((snd e) `notElem` (map snd regular)) =
        regularOrAside' es ((e:regular), aside)
    | (e `notElem` (Prelude.concat [regular, aside])) =
        regularOrAside' es (regular, (e:aside))
    | otherwise =
        regularOrAside' es (regular, aside)

edgesFromRoute :: Route -> [Edge]
edgesFromRoute r =
    edgesFromRoute' r []

edgesFromRoute' :: Route -> [Edge] -> [Edge]
edgesFromRoute' [] out =
    out
edgesFromRoute' (x:[]) out =
    out
edgesFromRoute' (x:y:xs) out =
    edgesFromRoute' (y:xs) ((x,y):out)

addLink :: [Edge] -> [Node] -> [Node]
addLink es ns =
    addLink' 0 es ns

addLink' :: Int -> [Edge] -> [Node] -> [Node]
addLink' _ [] nodes =
    nodes
addLink' count ((t,i):es) nodes =
    addLink' (count + 1) es nodes'
    where
        nodes' =
            map (\node -> if fst node == t then (fst node, (TailLink (pack $ show count) : snd node)) else node ) $
            map (\node -> if fst node == i then (fst node, (HeadLink (pack $ show count) : snd node)) else node ) nodes

deNode :: [Node] -> [(Int, [Text])]
deNode =
    map (\(x, as) -> (x, deNode' as))

deNode' :: [Attr] -> [Text]
deNode' = 
    map (LT.toStrict . B.toLazyText . deNode'') . Data.List.sort

deNode'' :: Attr -> B.Builder
deNode'' a = case a of
    AttrTaskId    i     -> B.singleton '#' <> B.decimal i
    IsDone              -> B.singleton '%'
    IsStarred           -> B.singleton '*'
    HeadLink      h     -> B.fromText h <> B.singleton ']'
    Title         t     -> B.fromText t
    StartableDate y m d -> B.decimal y <>  B.singleton '/' <> B.decimal m <> B.singleton '/' <> B.decimal d <> B.singleton '-'
    StartableTime h m   -> B.decimal h <>  B.singleton ':' <> B.decimal m <> B.singleton '-'
    Weight        w     -> B.singleton '$' <> fixed 1 w
    DeadlineDate  y m d -> B.singleton '-' <> B.decimal y <>  B.singleton '/' <> B.decimal m <> B.singleton '/' <> B.decimal d
    DeadlineTime  h m   -> B.singleton '-' <> B.decimal h <>  B.singleton ':' <> B.decimal m
    AssignName    a     -> B.singleton '@' <> B.fromText a
    TailLink      t     -> B.singleton '[' <> B.fromText t
    Link          l     -> B.singleton '&' <> B.fromText l

markDown :: Graph -> [(Int, Indent)]
markDown g =
    let
        (_,tree) = g
        ts = trunks g
    in
        Prelude.concatMap (\t-> markDown' t (pred_ t tree) [] tree tree 0 [(t,0)]) ts

markDown' :: Int -> [Int] -> [Int] -> [Edge] -> [Edge] -> Indent -> [(Int, Indent)] -> [(Int, Indent)]
markDown' _ _ [] [] _ _ out =
    out
markDown' current (x:xs) remain tre tree depth out =
    markDown' x (pred_ x tre) (current:remain) (explored current x tre) tree (depth + 1) ((x, depth + 1):out)
markDown' current [] (r:rs) tre tree depth out =
    markDown' r (pred_ r tre) rs tre tree (depth - 1) out

pred_ :: Int -> [Edge] -> [Int]
pred_ x =
    (map snd) . (filter (\(t,_) -> t == x))

succ_ :: Int -> [Edge] -> [Int]
succ_ y =
    (map fst) . (filter (\(_,i) -> i == y))

explored :: Int -> Int -> [Edge] -> [Edge]
explored v x =
    filter (\(t,i) -> (t,i) /= (v,x))

bindLines :: [(Int, Indent)] -> [(Int, [Text])] -> Text
bindLines indexedIndents indexedWordss = 
    let
        indexedTexts = map (\(i,ws) -> (i, Data.Text.unwords ws)) indexedWordss
        pairs = bindLines' indexedIndents indexedTexts []
    in
        Data.Text.unlines $ map (\(i,t) -> Data.Text.concat [Data.Text.replicate i indent, t]) pairs

bindLines' :: [(Int, Indent)] -> [(Int, Text)] -> [(Indent, Text)] -> [(Indent, Text)]
bindLines' [] _ out =
    out
bindLines' ((i,ind):pairs) indexedTexts out =
    case Data.List.find (\(j,_) -> i == j) indexedTexts of
        Just (_,t) ->
            bindLines' pairs indexedTexts ((ind,t):out)
        _ ->
            bindLines' pairs indexedTexts out

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
        Task d s ml ms md mw mt _ = entityVal task
        ems = toElmTime ms
        emd = toElmTime md
        eu  = Q.unValue assignName
        esch = map toElmSchedule schedules
    in
        ElmTask i d s mt ml ems emd mw eu esch

toElmUser :: Entity User -> [Entity Duration] -> ElmUser
toElmUser eu ers =
    let
        i = idFromEntity eu
        User n _ _ _ _ dot _ = entityVal eu
    in
        ElmUser i n dot Nothing Nothing

toElmTime :: Maybe UTCTime -> Maybe Millis
toElmTime Nothing = 
    Nothing
toElmTime (Just t) =
    Just (floor $ 10^3 * utcTimeToPOSIXSeconds t)

timeZoneHour :: ElmUser -> TimeZoneHour
timeZoneHour _ = 9  -- TODO timeZoneHour

millisFromWeight :: Double -> Millis
millisFromWeight w = floor $ 60 * 60 * 10^3 * w

type MillisDuration = (Millis, Millis)

toMillis :: Duration -> MillisDuration
toMillis (Duration l r _) = (fromTOD l, fromTOD r)

fromTOD :: TimeOfDay -> Millis
fromTOD t = floor $ (10^3) * (timeOfDayToTime t)

millisFromUTC ::UTCTime -> Millis
millisFromUTC u = floor . ((*) (10^3)) . utcTimeToPOSIXSeconds $ u

utcFromMillis :: Millis -> UTCTime
utcFromMillis = posixSecondsToUTCTime . fromIntegral. (`div` 10^3)  

buildOkMsg :: [a] -> String -> ElmMessage
buildOkMsg xs str =
    let body = pack $ (show $ Prelude.length xs) ++ str
    in  ElmMessage 200 body

isTrunk :: [Edge] -> Int -> Bool
isTrunk es t =
    Prelude.all (\(_,i) -> t /= i) es

isBud :: [Edge] -> Int -> Bool
isBud es i =
    Prelude.all (\(t,_) -> i /= t) es

toElmSchedule :: Schedule -> ElmSchedule
toElmSchedule (Schedule b e _) = 
    ElmSchedule (millisFromUTC b) (millisFromUTC e)

scheduleForward :: User -> UTCTime -> [Duration] -> [Entity Task] -> [Path] -> Either ElmMessage [Schedule]
scheduleForward user now ds tasks paths =
    let
        today = midnightBy now
        universal = (-1)*60*60*(10^3)*(userTimeZone user)
        daily = (map (both $ (+) universal)) . (map toMillis) $ ds
        pattern = stripedPattern (map (both $ (+) today) daily)
        reso = 60 * (10^3) * (userResolutionMin user)
        cursor = millisFromUTC now
        frags = map toTaskFrag tasks
        edges = map edgeFromPath paths
        roll = pianoRollF pattern reso cursor frags edges []
    in
    if  hasLoop edges then
        Left $ ElmMessage 400 "Not applicable to diagrams with loops."
    else
        Right $ toSchedule tasks roll

hasLoop :: [Edge] -> Bool
hasLoop es =
    Prelude.any (\x -> hasLoopObserve x (succ_ x es) [] (map (\v -> (v, x)) (succ_ x es)) es [x]) (allVertex es)

allVertex :: [Edge] -> [Int]
allVertex es =
    nub $ Prelude.concat [map fst es, map snd es]

hasLoopObserve :: Int -> [Int] -> [Int] -> [Edge] -> [Edge] -> Route -> Bool
hasLoopObserve _ [] [] _ _ stack =
    False
hasLoopObserve current (x:xs) remain cha chart stack =
    hasLoopExplore current (x:xs) remain (lightenSucc cha chart current x) chart stack
hasLoopObserve current [] (r:remain) cha chart stack =
    hasLoopExplore current [] (r:remain) cha chart stack

hasLoopExplore :: Int -> [Int] -> [Int] -> [Edge] -> [Edge] -> Route -> Bool
hasLoopExplore _ [] [] _ _ stack =
    False
hasLoopExplore current (x:[]) remain cha chart stack =
    if x `elem` stack then True else
    hasLoopObserve x (succ_ x cha) remain cha chart (x:stack)
hasLoopExplore current (x:_) remain cha chart stack =
    if x `elem` stack then True else
    hasLoopObserve x (succ_ x cha) (current:remain) cha chart (x:stack)
hasLoopExplore current [] (r:remain) cha chart stack =
    hasLoopObserve r (succ_ r cha) remain cha chart (Prelude.dropWhile (/= r) stack)

midnightBy :: UTCTime -> Millis
midnightBy t = millisFromUTC $ addUTCTime  (fromRational $ (-1)*(toRational $ utctDayTime t)) t

stripedPattern :: [MillisDuration] -> [MillisDuration]
stripedPattern ds =
    let oneDay = floor $ (10^3) * nominalDay
    in  Prelude.concat [ds, (map (both $ (+) oneDay) (stripedPattern ds))]
 
data TaskFrag = TaskFrag
    { fId :: Int
    , fStartable :: Maybe Millis
    , fDeadline :: Maybe Millis
    , fWeight :: Maybe Millis
    } deriving (Eq, Ord, Show)

toTaskFrag :: Entity Task -> TaskFrag
toTaskFrag e =
    let
        fid = idFromEntity e
        t = entityVal e
        startable = millisFromUTC <$> (taskStartable t)
        deadline = millisFromUTC <$> (taskDeadline t)
        weight = millisFromWeight <$> (taskWeight t)
    in
        TaskFrag fid startable deadline weight

toSchedule :: [Entity Task] -> [(TaskFrag, [MillisDuration])] -> [Schedule]
toSchedule ts ss =
    toSchedule' ts ss []

toSchedule' :: [Entity Task] -> [(TaskFrag, [MillisDuration])] -> [Schedule] -> [Schedule]
toSchedule' [] _ _ =
    []
toSchedule' _ [] out =
    out
toSchedule' ts ((f,ds):ss) out =
    case findTaskById ts (fId f) of
        Nothing ->
            toSchedule' ts ss out
        Just task ->
            let
                schedules = map (\(l,r) -> Schedule (utcFromMillis l) (utcFromMillis r) (entityKey task)) ds
            in  toSchedule' ts ss (Prelude.concat [schedules, out])

findTaskById :: [Entity Task] -> Int -> Maybe (Entity Task)
findTaskById es i =
    Data.List.find (\e -> i == idFromEntity e) es

pianoRollF :: [MillisDuration] -> Millis -> Millis -> [TaskFrag] -> [Edge] -> [(TaskFrag, [MillisDuration])] -> [(TaskFrag, [MillisDuration])]
pianoRollF pattern reso cursor frags edges out 
    | Prelude.null $ filterW frags =
        out
    | Prelude.null $ filterS frags =
        pianoRollF pattern reso next frags edges out
    | otherwise =
        pianoRollF pattern reso next newFrags edges ((winner,reward):out)
    where
        filterW = 
            filter (hasDirectOrIndirectDeadline frags edges) .
            filter (hasWeight)
        filterS =
            filter (isExecutableF cursor frags edges) .
            filterW
        entry =
            filterS frags
        winner =
            maximumBy (\f g -> compare (urgencyF cursor frags edges f) (urgencyF cursor frags edges g)) entry
        reward =
            assignTimeF pattern reso cursor
        next =
            Prelude.maximum $ map snd reward
        newFrags =
            weightDown reso frags winner 

hasDirectOrIndirectDeadline :: [TaskFrag] -> [Edge] -> TaskFrag -> Bool
hasDirectOrIndirectDeadline frags edges f =
    Prelude.any (hasDeadline) (successors frags edges f) || hasDeadline f

hasDeadline :: TaskFrag -> Bool
hasDeadline f =
    fDeadline f /= Nothing

successors :: [TaskFrag] -> [Edge] -> TaskFrag -> [TaskFrag]
successors fs es f =
    matchFrags fs (successors' es (fId f))

successors' :: [Edge] -> Int -> [Int]
successors' es x =
    sortUniq $ successors'' es x []

successors'' :: [Edge] -> Int -> [Int] -> [Int]
successors'' es x out 
    | Prelude.null neighborhoods =
        out
    | otherwise =
        Prelude.concat $ map (\y -> successors'' es y (Prelude.concat [neighborhoods,out])) neighborhoods 
        where
            neighborhoods = map fst $ filter (\(_,i) -> i == x) es

predecessors :: [TaskFrag] -> [Edge] -> TaskFrag -> [TaskFrag]
predecessors fs es f =
    matchFrags fs (predecessors' es (fId f))

predecessors' :: [Edge] -> Int -> [Int]
predecessors' es x =
    sortUniq $ predecessors'' es x []

predecessors'' :: [Edge] -> Int -> [Int] -> [Int]
predecessors'' es x out 
    | Prelude.null neighborhoods =
        out
    | otherwise =
        Prelude.concat $ map (\y -> predecessors'' es y (Prelude.concat [neighborhoods,out])) neighborhoods 
        where
            neighborhoods = map snd $ filter (\(t,_) -> t == x) es

hasWeight :: TaskFrag -> Bool
hasWeight f =
    case fWeight f of
        Just w ->
            w > 0
        _ ->
            False

isExecutableF :: Millis -> [TaskFrag] -> [Edge] -> TaskFrag -> Bool
isExecutableF cursor frags edges f =
    (case fStartable f of
        Just s ->
            s <= cursor
        _ ->
            True
    ) && Prelude.all (\frag -> not $ hasWeight frag) (predecessors frags edges f)

urgencyF :: Millis -> [TaskFrag] -> [Edge] -> TaskFrag -> Maybe Millis
urgencyF cursor frags edges f =
    let routes = routeFindF frags edges f
    in  Prelude.maximum $ map (urgencyByRouteF cursor) routes

urgencyByRouteF :: Millis -> [TaskFrag] -> Maybe Millis
urgencyByRouteF cursor route
    | Prelude.null $ filter (hasDeadline) route =
        Nothing
    | isNothing $ grandDeadline route =
        Nothing
    | 0 >= totalWeight route =
        Nothing
    | otherwise =
        let Just gd = grandDeadline route
        in  Just $ cursor + (totalWeight route) - gd

grandDeadline :: [TaskFrag] -> Maybe Millis
grandDeadline =
    Prelude.maximum . (map fDeadline) . (filter hasDeadline)

totalWeight :: [TaskFrag] -> Millis
totalWeight  =
    sum . map   (\frag -> case fWeight frag of 
                    Nothing ->
                        0
                    Just w ->
                        w
                ) 

routeFindF :: [TaskFrag] -> [Edge] -> TaskFrag -> [[TaskFrag]]
routeFindF frags edges f =
    let
        x = fId f
        routes = routeObserve x (succ_ x edges) [] (map (\v -> (v, x)) (succ_ x edges)) edges [x] []
    in
        map (matchFrags frags) routes

matchFrags :: [TaskFrag] -> [Int] -> [TaskFrag]
matchFrags fs xs =
    let
        mfs = map (\x -> Data.List.find (\f -> fId f == x) fs) xs
    in
        if Prelude.all (/= Nothing) mfs then
            map fromJust mfs
        else
            []

assignTimeF :: [MillisDuration] -> Millis -> Millis -> [MillisDuration]
assignTimeF pattern reso cursor =
    assignTimeF' pattern reso cursor reso []

assignTimeF' :: [MillisDuration] -> Millis -> Millis -> Millis -> [MillisDuration] -> [MillisDuration]
assignTimeF' [] _ _ _ _ = []
assignTimeF' ((l,r):(l',r'):pattern) reso cursor rest out
    | cursor < l =
        assignTimeF' ((l,r):(l',r'):pattern) reso l rest out
    | l <= cursor && cursor < r && r < cursor + rest =
        assignTimeF' ((l',r'):pattern) reso l' (cursor + rest - r) ((cursor,r):out)
    |  l <= cursor && cursor < r =
        (cursor, cursor + reso) : out
    | otherwise =
        assignTimeF' ((l',r'):pattern) reso cursor rest out

weightDown :: Millis -> [TaskFrag] -> TaskFrag -> [TaskFrag]
weightDown reso frags winner =
    map (\frag -> if frag == winner then 
        let TaskFrag x ms md mw = winner
        in  TaskFrag x ms md ((-) <$> mw <*> Just reso)
        else
            frag
        ) frags

isCommand :: Text -> Bool
isCommand "" = False
isCommand t = Data.Text.head t == '/'

aSlashCmd :: Parser SlashCmd
aSlashCmd = 
        SlashSel     <$ string "/sel "     <*> takeText
    <|> SlashDot     <$ string "/dot "     <*> takeText
    <|> SlashAllow   <$ string "/allow "   <*> takeTill (==' ') <* char ' ' <*> takeTill (==' ') <* char ' ' <*> takeText
    <|> SlashBan     <$ string "/ban "     <*> takeTill (==' ') <* char ' ' <*> takeTill (==' ') <* char ' ' <*> takeText

aCondition :: Parser Condition
aCondition = 
        SelTitle        <$ string "t "     <*> takeText
    <|> SelNotTitle     <$ string "nt "    <*> takeText
    <|> SelStartableL   <$ string "s "     <*> decimal <* char '/' <*> decimal <* char '/' <*> decimal <* char '_' <*> decimal <* char ':' <*> decimal <* char '<'
    <|> SelStartableR   <$ string "s "     <* char '<' <*> decimal <* char '/' <*> decimal <* char '/' <*> decimal <* char '_' <*> decimal <* char ':' <*> decimal
    <|> SelDeadlineL    <$ string "d "     <*> decimal <* char '/' <*> decimal <* char '/' <*> decimal <* char '_' <*> decimal <* char ':' <*> decimal <* char '<'
    <|> SelDeadlineR    <$ string "d "     <* char '<' <*> decimal <* char '/' <*> decimal <* char '/' <*> decimal <* char '_' <*> decimal <* char ':' <*> decimal
    <|> SelWeightL      <$ string "w "     <*> double <* char '<'
    <|> SelWeightR      <$ string "w "     <* char '<' <*> double
    <|> SelAssign       <$ string "a "     <*> takeText
    <|> SelNotAssign    <$ string "na "    <*> takeText
    <|> SelArchived     <$ string "arc"
    <|> SelUndone       <$ string "und"
    <|> SelStarred      <$ string "sta"
    <|> SelTrunks       <$ string "tru"
    <|> SelBuds         <$ string "bud"
    <|> SelRelationL    <$ string "r "     <*> decimal <* char '<'
    <|> SelRelationR    <$ string "r "     <* char '<' <*> decimal

alterEdge :: Int -> TaskId -> [Edge] -> [Edge]
alterEdge x tid =
    map (\(t,i) -> if t == x then (idFromKey tid,i) else (t,i)) .
    map (\(t,i) -> if i == x then (t,idFromKey tid) else (t,i))
