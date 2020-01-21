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

import Data.Time.Clock (nominalDay)
import Data.List (sort, maximumBy, intersect)
import Data.Text.Format (fixed)
import Data.List.Unique (allUnique, sortUniq)
import Data.Maybe (isNothing)

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
    | SlashCare Int Int
    | SlashAllow Text Text Text
    | SlashBan Text Text Text
    | SlashConnect Text Text

data Condition
    = SelLike Text
    | SelNotLike Text 
    | SelStartableL Int Int Int Int Int
    | SelStartableR Int Int Int Int Int
    | SelStartableLR Int Int Int Int Int Int Int Int Int Int
    | SelDeadlineL Int Int Int Int Int
    | SelDeadlineR Int Int Int Int Int
    | SelDeadlineLR Int Int Int Int Int Int Int Int Int Int
    | SelWeightL Double
    | SelWeightR Double
    | SelWeightLR Double Double
    | SelAssign Text
    | SelArchived
    | SelStarred
    | SelTrunks
    | SelBuds
    | SelRelationL Int 
    | SelRelationR Int
    | SelRelationLR Int Int
  


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
    :<|> undoneTasksReload
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
        showTrunk :: ElmUser -> Handler ElmSubModel
        showTrunk = liftIO . showTrunk'
        showBuds :: ElmUser -> Handler ElmSubModel
        showBuds = liftIO . showBuds'

