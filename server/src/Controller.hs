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
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
-- import Data.Time.Clock.Internal.NominalDiffTime (nominalDiffTimeToSeconds)
import qualified Database.Esqueleto as Q (Value (..)) 
-- import Data.Int                     ( Int64 )

import Data.Time.Clock (nominalDay)
import Data.List (sort)
import Data.Text.Format (fixed, prec)

-- type alias



-- yeaPerY = 1
-- quaPerY = 4
-- monPerY = 12
-- weePerY = 52
-- dayPerY = 365
-- houPerY = 8760
-- minPerY = 525600
-- secPerY = 31536000

anonymousUser = toSqlKey 1 :: UserId

-- data ZoneName =
--     Name String
--     | Offset Int

type TimeZoneHour = Int
type Millis = Int
type Minutes = Int


-- DATA DECLARATION



data ElmDuration = ElmDuration
    { elmDurationLeft   :: Millis
    , elmDurationRight  :: Millis
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmDuration)

data ElmUser = ElmUser
    { elmUserId         :: Int
    , elmUserName       :: Text
    , elmUserAdmin      :: Bool
    , elmUserDurations  :: [ElmDuration]
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
    , elmTaskBegin      :: Maybe Millis
    , elmTaskEnd        :: Maybe Millis
    , elmTaskDeadline   :: Maybe Millis
    , elmTaskWeight     :: Maybe Double
    , elmTaskUser       :: Text
    -- , elmTaskSecUntilStartable :: Maybe Integer
    -- , elmTaskSecUntilDeadline :: Maybe Integer
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ElmTask)

data ElmSubModel = ElmSubModel
    { elmSubModelUser       :: ElmUser
    , elmSubModelTasks      :: [ElmTask]
    , elmSubModelInputText  :: Maybe Text
    , elmSubModelMessage    :: Maybe Text
    }

$(deriveJSON defaultOptions ''ElmSubModel)

data Initial = Initial
    { initialUser :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Initial)

data TextPost = TextPost
    { textPostUser      :: ElmUser
    , textPostContent   :: Text
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''TextPost)

data DoneTasks = DoneTasks
    { doneTasksUser :: Int
    , doneTasksIds  :: [Int]
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''DoneTasks)

data SwitchStar = SwitchStar
    { switchStarId :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''SwitchStar)

data FocusTask = FocusTask
    { focusTaskId :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''FocusTask)

data GoHome = GoHome
    { goHomeUser :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''GoHome)

data CloneTasks = CloneTasks
    { cloneTasksUser :: ElmUser
    , cloneTasksIds :: [Int]
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''CloneTasks)



-- API DEFINITION



type API =  "dev"   :> "model"  :> Capture "user"  Int          :> Get  '[JSON] ElmSubModel
    :<|>    "tasks" :> "init"   :> ReqBody '[JSON] Initial      :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "post"   :> ReqBody '[JSON] TextPost     :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "done"   :> ReqBody '[JSON] DoneTasks    :> Post '[JSON] [ElmTask]
    :<|>    "tasks" :> "star"   :> ReqBody '[JSON] SwitchStar   :> Post '[JSON] ()
    :<|>    "tasks" :> "focus"  :> ReqBody '[JSON] FocusTask    :> Post '[JSON] [ElmTask]
    :<|>    "tasks" :> "home"   :> ReqBody '[JSON] GoHome       :> Post '[JSON] ElmSubModel
    :<|>    "tasks" :> "clone"  :> ReqBody '[JSON] CloneTasks   :> Post '[JSON] ElmSubModel

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
    where
        devSubModel :: Int -> Handler ElmSubModel
        devSubModel = liftIO . devSubModel'
        initialize :: Initial -> Handler ElmSubModel
        initialize = liftIO . initialize'
        textPostReload :: TextPost -> Handler ElmSubModel
        textPostReload = liftIO . textPostReload'
        doneTasksReload :: DoneTasks -> Handler [ElmTask]
        doneTasksReload = liftIO . doneTasksReload'
        switchStar :: SwitchStar -> Handler ()
        switchStar = liftIO . switchStar'
        focusTask :: FocusTask -> Handler [ElmTask]
        focusTask = liftIO . focusTask'
        goHome :: GoHome -> Handler ElmSubModel
        goHome = liftIO . goHome'
        cloneTasks :: CloneTasks -> Handler ElmSubModel
        cloneTasks = liftIO . cloneTasks'

devSubModel' :: Int -> IO ElmSubModel
devSubModel' uid = do
    pool <- pgPool
    user <- Prelude.head <$> getUserById pool uid
    durations <- getDurationsById pool uid
    taskAssigns <- getUndoneTaskAssigns pool uid
    let elmUser = toElmUser user durations
    let elmTasks = map toElmTask taskAssigns 
    return $ ElmSubModel elmUser elmTasks Nothing Nothing

initialize' :: Initial -> IO ElmSubModel
initialize' (Initial uid) =
    devSubModel' uid


getUndoneElmTasks :: Int -> IO [ElmTask]
getUndoneElmTasks user = do
    pool <- pgPool
    taskAssigns <- getUndoneTaskAssigns pool user
    return $ map toElmTask taskAssigns 

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
    durations <- (map entityVal) <$> getDurationsById pool uid
    let tasks = tasksFromText user durations text
    fault <- faultPost user tasks
    case fault of
        Just errMsg ->
            return $ ElmSubModel elmUser [] Nothing (Just errMsg) 
        Nothing -> do
            mn <- getMaxNode
            let maxNode = case mn of
                    Nothing -> 0
                    Just n  -> n
            let shift = maxNode + 2  -- TODO 
            insTasks . (shiftTaskNodes shift) $ tasks
            newTasks <- getUndoneTaskAssigns pool uid
            let elmTasks = map toElmTask newTasks
            let okMsg = buildOkMsg tasks " tasks registered."
            return $ ElmSubModel elmUser elmTasks Nothing (Just okMsg)

faultPost :: User -> [Task] -> IO (Maybe Text)
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

goHome' :: GoHome -> IO ElmSubModel
goHome' (GoHome uid) = 
    devSubModel' uid

cloneTasks' :: CloneTasks -> IO ElmSubModel
cloneTasks' (CloneTasks elmUser taskIds) = do
    pool <- pgPool
    let uid = elmUserId elmUser
    fault <- faultClonePermission uid taskIds
    case fault of
        Just errMsg ->
            return $ ElmSubModel elmUser [] Nothing (Just errMsg) 
        Nothing -> do
            user <- Prelude.head <$> (map entityVal) <$> getUserById pool uid
            -- durations <- (map entityVal) <$> getDurationsById pool uid
            taskAssigns <- (map (\(e,v) -> (entityVal e, (entityKey e, Q.unValue v)))) <$> getTaskAssignsByIds taskIds
            let text = textFromTasks user taskAssigns
            let okMsg = buildOkMsg taskAssigns " tasks cloned."
            return $ ElmSubModel elmUser [] (Just text) (Just okMsg)

faultClonePermission :: Int -> [Int] -> IO (Maybe Text)
faultClonePermission uid [] = return $ Nothing
faultClonePermission uid (t:ts) = do
    perm <- hasEditPerm uid t
    if perm then
        faultClonePermission uid ts
    else do
        pool <- pgPool
        taskAssign <- Prelude.head <$> getTaskAssignById pool t
        return $ Just (buildCloneErrMsg taskAssign)

getTaskAssignsByIds :: [Int] -> IO [(Entity Task, Q.Value Text)]
getTaskAssignsByIds ids = do
    pool <- pgPool
    Prelude.concat <$> sequence ( map (getTaskAssignById pool) ids)
    

hasEditPerm :: Int -> Int -> IO (Bool)
hasEditPerm uid taskId =
    -- TODO
    return True 



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
    | StartableTime { startableHou  :: Int, startableMin    :: Int, startableSec    :: Int }
    | Weight        { weight        :: Double }
    | DeadlineDate  { deadlineYea   :: Int, deadlineMon     :: Int, deadlineDay     :: Int }
    | DeadlineTime  { deadlineHou   :: Int, deadlineMin     :: Int, deadlineSec     :: Int }
    | Assign        { assign        :: Text }
    | TailLink      { tailLink      :: Text }
    | Link          { link          :: Text }
    deriving (Eq, Show, Ord)

tasksFromText :: User -> [Duration] -> Text -> [Task]
tasksFromText u ds = 
    (tasksFromGraph u ds) . graphFromText

tasksFromGraph :: User -> [Duration] -> Graph -> [Task]
tasksFromGraph u ds = 
    (map (universalize u)) . (setBeginEnd u ds) . (map taskFromEdge)

taskFromEdge :: Edge -> Task
taskFromEdge ((t,i),as) =
    taskFromEdge' as (Task t i False False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing anonymousUser)

taskFromEdge' :: [Attr] -> Task -> Task
taskFromEdge' [] task = 
    task
taskFromEdge' (a:as) (Task t i y d s ml ms mb me md mw mt u) =
    case a of
        AttrTaskId 0 ->
            taskFromEdge' as (Task t i y d s ml ms mb me md mw mt u)
        AttrTaskId n ->  -- TODO
            taskFromEdge' as (Task t i y d s ml ms mb me md mw mt u)
        IsDone ->
            taskFromEdge' as (Task t i y True s ml ms mb me md mw mt u)
        IsStarred ->
            taskFromEdge' as (Task t i y d True ml ms mb me md mw mt u)
        Link l ->
            taskFromEdge' as (Task t i y d s (Just l) ms mb me md mw mt u)
        StartableDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case ms of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as (Task t i y d s ml (Just (UTCTime nd ot)) mb me md mw mt u)
                    Nothing ->
                        taskFromEdge' as (Task t i y d s ml (Just (UTCTime nd 0)) mb me md mw mt u)
        StartableTime hh mm ss ->
            let
                nt = secondsToDiffTime . fromIntegral $ ss + 60 * (mm + 60 * hh)
            in
                case ms of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as (Task t i y d s ml (Just (UTCTime od nt)) mb me md mw mt u)
                    Nothing ->
                        let
                            nd = fromGregorian 3000 0 0
                        in
                            taskFromEdge' as (Task t i y d s ml (Just (UTCTime nd nt)) mb me md mw mt u)
        DeadlineDate yyyy mm dd ->
            let
                nd = fromGregorian (fromIntegral yyyy) mm dd
            in
                case md of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as (Task t i y d s ml ms mb me (Just (UTCTime nd ot)) mw mt u)
                    Nothing ->
                        taskFromEdge' as (Task t i y d s ml ms mb me (Just (UTCTime nd 0)) mw mt u)
        DeadlineTime hh mm ss ->
            let
                nt = secondsToDiffTime . fromIntegral $ ss + 60 * (mm + 60 * hh)
            in
                case md of
                    Just (UTCTime od ot) ->
                        taskFromEdge' as (Task t i y d s ml ms mb me (Just (UTCTime od nt)) mw mt u)
                    Nothing ->
                        let
                            nd = fromGregorian 3000 0 0
                        in
                            taskFromEdge' as (Task t i y d s ml ms mb me (Just (UTCTime nd nt)) mw mt u)
        Weight w ->
            taskFromEdge' as (Task t i y d s ml ms mb me md (Just w) mt u)
        -- Assign an ->  --TODO
        Title tt' ->
            case mt of
                Just tt ->
                    taskFromEdge' as (Task t i y d s ml ms mb me md mw (Just $ Data.Text.intercalate " " [tt', tt]) u)
                Nothing ->
                    taskFromEdge' as (Task t i y d s ml ms mb me md mw (Just tt') u)
        _ -> 
            taskFromEdge' as (Task t i y d s ml ms mb me md mw mt u)

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
    <|> StartableTime <$> decimal   <*  char ':' <*> decimal <* char ':' <*> decimal <* char '-'
    <|> DeadlineDate  <$  char '-'  <*> decimal  <* char '/' <*> decimal <* char '/' <*> decimal
    <|> DeadlineTime  <$  char '-'  <*> decimal  <* char ':' <*> decimal <* char ':' <*> decimal
    <|> Weight        <$  char '$'  <*> double
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
timeShift diff (Task t i y d s l ms mb me md mw tt u) = 
    let
        msS = addUTCTime <$> Just diff <*> ms
        mbS = addUTCTime <$> Just diff <*> mb
        meS = addUTCTime <$> Just diff <*> me
        mdS = addUTCTime <$> Just diff <*> md
    in
        Task t i y d s l msS mbS meS mdS mw tt u

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
edgeFromTask ((Task t i y d s ml ms _ _ md mw mt _), (id,an)) =
    let
        (sY, sM, sD) = case ms of 
            Just (UTCTime s _)  -> toGregorian s
            Nothing             -> (0,0,0)
        (sh, sm, ss) = case ms of 
            Just (UTCTime _ s)  -> toHMS s
            Nothing             -> (0,0,0)
        (dY, dM, dD) = case ms of 
            Just (UTCTime d _)  -> toGregorian d
            Nothing             -> (0,0,0)
        (dh, dm, ds) = case ms of 
            Just (UTCTime _ d)  -> toHMS d
            Nothing             -> (0,0,0)
        attr = shave
            [ AttrTaskId    (fromIntegral . fromSqlKey $ id)
            , IsDone
            -- , IsDone        (if d then '%' else '_')
            , IsStarred
            -- , IsStarred     (if s then '*' else '_')
            -- , HeadLink      
            , Title         (case mt of
                                Just tt  -> tt
                                Nothing -> "")
            , StartableDate (fromIntegral sY) sM sD
            , StartableTime sh sm ss
            , Weight        (case mw of
                                Just w  -> w
                                Nothing -> 0)
            , DeadlineDate  (fromIntegral dY) dM dD
            , DeadlineTime  dh dm ds
            , Assign        an
            -- , TailLink      
            , Link          (case ml of
                                Just l  -> l
                                Nothing -> "")
            ]
    in
        ((t,i), attr)

shave :: [Attr] -> [Attr]
shave attrs = attrs  -- TODO

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
    StartableTime h m s ->
        B.decimal h <>  B.singleton ':' <> B.decimal m <> B.singleton ':' <> B.decimal s <> B.singleton '-'
    Weight        w     -> B.singleton '$' <> prec 4 w
    DeadlineDate  y m d ->
        B.singleton '-' <> B.decimal y <>  B.singleton '/' <> B.decimal m <> B.singleton '/' <> B.decimal d
    DeadlineTime  h m s ->
        B.singleton '-' <> B.decimal h <>  B.singleton ':' <> B.decimal m <> B.singleton ':' <> B.decimal s
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

isTrunkNode :: Node -> [(Node, Node)] -> Bool 
isTrunkNode node tree =
    Prelude.null (prevNodes node tree)

isBudNode :: Node -> [(Node, Node)] -> Bool 
isBudNode node tree =
    Prelude.null (nextNodes node tree)

renumberTrunk :: Node -> [(Node, Node)] -> [(Node, Node)]
renumberTrunk n tree =
    map (\(t,i) -> if isTrunkNode t tree then (n,i) else (t,i)) tree

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

toElmTask :: (Entity Task, Q.Value Text) -> ElmTask
toElmTask (e, u) =
    let
        i  = idFromEntity e
        Task _ _ y d s ml ms mb me md mw mt _ = entityVal e
        ems = toElmTime ms
        emb = toElmTime mb
        eme = toElmTime me
        emd = toElmTime md
        eu  = Q.unValue u
    in
        ElmTask i y d s mt ml ems emb eme emd mw eu

toElmUser :: Entity User -> [Entity Duration] -> ElmUser
toElmUser eu ers =
    let
        i = idFromEntity eu
        User n a _ _ mdpy _ _ _ = entityVal eu
        durs = map toElmDuration ers 
    in
        ElmUser i n a durs mdpy Nothing Nothing

toElmTime :: Maybe UTCTime -> Maybe Millis
toElmTime Nothing = 
    Nothing
toElmTime (Just t) =
    Just (floor $ 10^3 * utcTimeToPOSIXSeconds t)

toElmDuration :: Entity Duration -> ElmDuration
toElmDuration e = 
    let
        Duration l r _ = entityVal e
    in
        ElmDuration l r 

shiftTaskNodes :: Int -> [Task] -> [Task]
shiftTaskNodes sh ts =
    map (
        \(Task t i y d s l ms mb me md w tt u) -> 
            let 
                (t',i') = both ((+) sh) (t,i)
            in
                Task t' i' y d s l ms mb me md w tt u
        ) ts

timeZoneHour :: ElmUser -> TimeZoneHour
timeZoneHour _ = 9  -- TODO

setBeginEnd :: User -> [Duration] -> [Task] -> [Task]
setBeginEnd u ds = 
    map (setBeginEnd' u ds)

setBeginEnd' :: User -> [Duration] -> Task -> Task
setBeginEnd' user ds (Task t i y d s ml ms mb me md mw mt u) =
    case mw of
        Nothing ->
            Task t i y d s ml ms mb me md mw mt u
        Just w ->
            let
                weight = millisFromWeight w
            in
            if  userIsLazy user then
                let
                    end = md
                    begin = case end of
                        Just e ->
                            beginFromEnd e ds weight
                        _ ->
                            Nothing
                in
                Task t i y d s ml ms begin end md mw mt u 
                
            else
                let
                    begin = ms
                    end = case begin of
                        Just b ->
                            endFromBegin b ds weight
                        _ ->
                            Nothing
                in
                Task t i y d s ml ms begin end md mw mt u 

millisFromWeight :: Double -> Millis
millisFromWeight w = floor $ 60 * 60 * 10^3 * w

type MillisDuration = (Millis, Millis)

toMillis :: Duration -> MillisDuration
toMillis = \(Duration l r _) -> (10^3*l,10^3*r)

millisFromUTC ::UTCTime -> Millis
millisFromUTC u = floor . ((*) (10^3)) . utcTimeToPOSIXSeconds $ u

utcFromMillis :: Millis -> UTCTime
utcFromMillis = posixSecondsToUTCTime . fromIntegral. (`div` 10^3)  

endFromBegin :: UTCTime -> [Duration] -> Millis -> Maybe UTCTime
endFromBegin begin ds weight =
    let
        ds' = map toMillis ds
        begin' = millisFromUTC begin
        diff = posi ds' ds' begin' weight
    in
    utcFromMillis <$> ((+) <$> (Just begin') <*> diff)

beginFromEnd :: UTCTime -> [Duration] -> Millis -> Maybe UTCTime
beginFromEnd end ds weight = 
    let
        ds' = reverse $ map toMillis ds
        end' = millisFromUTC end
        diff = posi' [] ds' end' weight
    in
    utcFromMillis <$> ((+) <$> (Just end') <*> diff)

millisPerDay :: Millis
millisPerDay = floor $ 10^3 * nominalDay

isInDur :: Millis -> MillisDuration -> Bool
isInDur t (l,r) =
    l <= t && t<= r 

dayPlus :: [MillisDuration] -> [MillisDuration]
dayPlus = map (both (+ millisPerDay) )

posi :: [MillisDuration] -> [MillisDuration] -> Millis -> Millis -> Maybe Millis
posi  _ [] _ _ = 
    Nothing
posi [] seed left rest =
    posi (dayPlus seed) (dayPlus seed) left rest
posi (d:ds) seed left rest
    | left `isInDur` d && (left + rest) `isInDur` d =
        Just (left + rest)
    | left `isInDur` d =
        nega ds seed (snd d) (rest - (snd d - left)) 
    | otherwise =
        nega (d:ds) seed left rest

nega :: [MillisDuration] -> [MillisDuration] -> Millis -> Millis -> Maybe Millis
nega  _ [] _ _ = 
    Nothing
nega [] seed left rest =
    nega (dayPlus seed) (dayPlus seed) left rest
nega (d:ds) seed left rest
    | left `isInDur` d =
        posi (d:ds) seed left rest
    | otherwise =
        posi (d:ds) seed (fst d) rest

dayMinus :: [MillisDuration] -> [MillisDuration]
dayMinus = map (both (\t -> t - millisPerDay))

posi' :: [MillisDuration] -> [MillisDuration] -> Millis -> Millis -> Maybe Millis
posi' _ [] _ _ = 
    Nothing
posi' [] seed right rest =
    posi' (dayMinus seed) (dayMinus seed) right rest
posi' (d:ds) seed right rest
    | right `isInDur` d && (right - rest) `isInDur` d =
        Just (right - rest)
    | right `isInDur` d =
        nega' ds seed (fst d) (rest - (right - fst d)) 
    | otherwise =
        nega' (d:ds) seed right rest

nega' :: [MillisDuration] -> [MillisDuration] -> Millis -> Millis -> Maybe Millis
nega' _ [] _ _ = 
    Nothing
nega' [] seed right rest =
    nega' (dayMinus seed) (dayMinus seed) right rest
nega' (d:ds) seed right rest
    | right `isInDur` d =
        posi' (d:ds) seed right rest
    | otherwise =
        posi' (d:ds) seed (snd d) rest

buildOkMsg :: [a] -> String -> Text
buildOkMsg xs str =
    pack $ (show $ Prelude.length xs) ++ str

buildPostErrMsg :: [Maybe Text] -> Text
buildPostErrMsg []              = "No fault."
buildPostErrMsg ((Just text):_) = text
buildPostErrMsg (Nothing:es)  = buildPostErrMsg es

buildCloneErrMsg :: (Entity Task, Q.Value Text) -> Text
buildCloneErrMsg (_, Q.Value userName) =
    Data.Text.concat [pack "You have no permission for @", userName]

