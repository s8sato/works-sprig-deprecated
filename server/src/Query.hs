{-# LANGUAGE OverloadedStrings #-}

module Query where

import Entity
import Database.Esqueleto
import Database.Persist.Sql         ( ConnectionPool
                                    , runSqlPool
                                    , Entity (..)
                                    , Key (..)
                                    , insert
                                    )
import Data.Time

import Control.Monad.IO.Class       ( MonadIO )
import Data.Text                    ( Text )



insUser :: ConnectionPool -> User -> IO (Key User)
insUser pool user = do
    flip runSqlPool pool $ do
        insert $ user

insDur :: ConnectionPool -> Duration -> IO (Key Duration)
insDur pool duration = do
    flip runSqlPool pool $ do
        insert $ duration

insTask :: ConnectionPool -> Task -> IO (Key Task)
insTask pool task = do
    flip runSqlPool pool $ do
        insert $ task

insTasks :: ConnectionPool -> [Task] -> IO ()
insTasks pool ts = do
    flip runSqlPool pool $ do
        sequence_ . map insert $ ts

insPaths :: ConnectionPool -> [Path] -> IO ()
insPaths pool ps = do
    flip runSqlPool pool $ do
        sequence_ . map insert $ ps

insSchedules :: ConnectionPool -> [Schedule] -> IO ()
insSchedules pool schedules = do
    flip runSqlPool pool $ do
        sequence_ . map insert $ schedules

-- keyFromInt :: Integral a => a -> BackendKey SqlBackend
-- keyFromInt = SqlBackendKey . fromIntegral

setStarSwitched :: ConnectionPool -> Key Task -> IO ()
setStarSwitched pool tKey = flip runSqlPool pool $ do
    update $ \task -> do
        let isStarred = sub_select $ from $ \task -> do
                where_ 
                    (   task ^. TaskId ==. val tKey
                    )
                return $ task ^. TaskIsStarred
        set
            task [TaskIsStarred =. not_ isStarred]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

-- setStarSwitched :: ConnectionPool -> Key Task -> IO ()
-- setStarSwitched pool tKey = flip runSqlPool pool $ do
--     update $ \task -> do
--         let mIsStarred = subSelect $ from $ \task -> do
--                 where_ 
--                     (   task ^. TaskId ==. val tKey
--                     )
--                 return $ task ^. TaskIsStarred
--         case mIsStarred of
--             Just isStarred -> do
--                 set
--                     task [TaskIsStarred =. not_ isStarred]
--                 where_ 
--                     (   task ^. TaskId ==. val tKey
--                     )
--             _ -> 
--                 return ()

getBeforeMeByTask :: ConnectionPool -> Key Task -> IO [Entity Task]
getBeforeMeByTask pool me = flip runSqlPool pool $ do
    select $ from $ \(path `InnerJoin` task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        on  (path ^. PathInitial ==. task ^. TaskId)
        where_
            (   path ^. PathTerminal ==. val me
            )
        groupBy (task ^. TaskId)
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getMeByTask :: ConnectionPool -> Key Task -> IO [Entity Task]
getMeByTask pool me = flip runSqlPool pool $ do
    select $ from $ \task  -> do
        where_
            (   task ^. TaskId ==. val me
            )
        return (task)

getAfterMeByTask :: ConnectionPool -> Key Task -> IO [Entity Task]
getAfterMeByTask pool me = flip runSqlPool pool $ do
    select $ from $ \(schedule `RightOuterJoin` task `InnerJoin` path) -> do
        on  (task ^. TaskId ==. path ^. PathTerminal)
        on  (schedule ^. ScheduleTask ==. task ^. TaskId)
        where_
            (   path ^. PathInitial ==. val me
            )
        groupBy (task ^. TaskId)
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getUser :: ConnectionPool -> Key User -> IO [Entity User]
getUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \user -> do
        where_
            (   user ^. UserId ==. val uKey
            )
        return (user)

getDurationsByUser :: ConnectionPool -> Key User -> IO [Entity Duration]
getDurationsByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \duration -> do
        where_
            (   duration ^. DurationUser ==. val uKey
            )
        orderBy
            [ asc (duration ^. DurationLeft)
            ]
        return (duration)

getTrunks :: ConnectionPool -> IO [Entity Task]
getTrunks pool = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   notExists $ from $ \path ->
                where_
                    (   path ^. PathInitial ==. task ^. TaskId
                    )
            )
        return (task)

getBuds :: ConnectionPool -> IO [Entity Task]
getBuds pool = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   notExists $ from $ \path ->
                where_
                    (   path ^. PathTerminal ==. task ^. TaskId
                    )
            )
        return (task)

setTaskDone :: ConnectionPool -> Key Task -> IO ()
setTaskDone pool tKey = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskIsDone =. val (True)]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

setTaskUndone :: ConnectionPool -> Key Task -> IO ()
setTaskUndone pool tKey = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskIsDone =. val (False)]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

setTaskStarred :: ConnectionPool -> Key Task -> IO ()
setTaskStarred pool tKey = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskIsStarred =. val (True)]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

setTaskLink :: ConnectionPool -> Key Task -> Text -> IO ()
setTaskLink pool tKey l = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskLink =. val (Just l)]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

setTaskStartable :: ConnectionPool -> Key Task -> UTCTime -> IO ()
setTaskStartable pool tKey s = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskStartable =. val (Just s)]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

setTaskDeadline :: ConnectionPool -> Key Task -> UTCTime -> IO ()
setTaskDeadline pool tKey d = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskDeadline =. val (Just d)]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

setTaskWeight :: ConnectionPool -> Key Task -> Double -> IO ()
setTaskWeight pool tKey w = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskWeight =. val (Just w)]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

setTaskTitle :: ConnectionPool -> Key Task -> Text -> IO ()
setTaskTitle pool tKey tt = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskTitle=. val (Just tt)]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

setTaskAssign :: ConnectionPool -> Key Task -> UserId -> IO ()
setTaskAssign pool tKey  u = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskAssign =. val (u)]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

getOwnTasksByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getOwnTasksByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskAssign ==. val uKey
            )
        groupBy
            (task ^. TaskId) 
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getTaskAssignByTask :: ConnectionPool -> Key Task -> IO [(Entity Task, Value Text)]
getTaskAssignByTask pool tid = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on  (task ^. TaskAssign ==. user ^. UserId)
        where_
            (   task ^. TaskId ==. val tid
            )
        return (task, user ^. UserName)

getSchedulesByTask :: ConnectionPool -> Key Task -> IO [Entity Schedule]
getSchedulesByTask pool tid = flip runSqlPool pool $ do
    select $ from $ \schedule -> do
        where_
            (   schedule ^. ScheduleTask ==. val tid
            )
        orderBy
            [ asc (schedule ^. ScheduleBegin)
            ]
        return (schedule)

getAllTasksByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getAllTasksByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \(permission `RightOuterJoin` task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        on  (permission ^. PermissionObject ==. task ^. TaskAssign)
        where_
            (   permission ^. PermissionSubject ==. val uKey
            ||. task ^. TaskAssign ==. val uKey
            )
        groupBy
            (task ^. TaskId) 
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getUndoneTasks :: ConnectionPool -> IO [Entity Task]
getUndoneTasks pool = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   not_ $ task ^. TaskIsDone
            )
        groupBy
            (task ^. TaskId) 
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getDoneTasks :: ConnectionPool -> IO [Entity Task]
getDoneTasks pool = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskIsDone
            )
        groupBy
            (task ^. TaskId) 
        orderBy
            [ desc (task ^. TaskIsStarred)
            , desc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getStarredTasks :: ConnectionPool -> IO [Entity Task]
getStarredTasks pool = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskIsStarred
            )
        groupBy
            (task ^. TaskId) 
        orderBy
            [ desc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

delSchedulesByUser :: ConnectionPool -> Key User -> IO ()
delSchedulesByUser pool uKey = flip runSqlPool pool $ do
    delete $ from $ \schedule ->
        where_
            (   exists $ from $ \(task `InnerJoin` user) -> do
                    on  (task ^. TaskAssign ==. user ^. UserId)
                    where_
                        (   task ^. TaskAssign ==. val uKey
                        &&. task ^. TaskId ==. schedule ^. ScheduleTask
                        )
            )

selTitle :: ConnectionPool -> Text -> IO [Entity Task]
selTitle pool word = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskTitle `like` just ((%) ++. val word ++. (%))
            )
        return (task)

selNotTitle :: ConnectionPool -> Text -> IO [Entity Task]
selNotTitle pool word = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   not_ $ task ^. TaskTitle `like` just ((%) ++. val word ++. (%))
            )
        return (task)

selStartableL :: ConnectionPool -> UTCTime -> IO [Entity Task]
selStartableL pool utc = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskStartable >. just (val utc)
            )
        return (task)

selStartableR :: ConnectionPool -> UTCTime -> IO [Entity Task]
selStartableR pool utc = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskStartable <. just (val utc)
            )
        return (task)

selDeadlineL :: ConnectionPool -> UTCTime -> IO [Entity Task]
selDeadlineL pool utc = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskDeadline >. just (val utc)
            )
        return (task)

selDeadlineR :: ConnectionPool -> UTCTime -> IO [Entity Task]
selDeadlineR pool utc = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskDeadline <. just (val utc)
            )
        return (task)

selWeightL :: ConnectionPool -> Double -> IO [Entity Task]
selWeightL pool weight = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskWeight >. just (val weight)
            )
        return (task)

selWeightR :: ConnectionPool -> Double -> IO [Entity Task]
selWeightR pool weight = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskWeight <. just (val weight)
            )
        return (task)

selAssign :: ConnectionPool -> Text -> IO [Entity Task]
selAssign pool name = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on  (task ^. TaskAssign ==. user ^. UserId)
        where_
            (   user ^. UserName ==. val name
            )
        return (task)

selNotAssign :: ConnectionPool -> Text -> IO [Entity Task]
selNotAssign pool name = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on  (task ^. TaskAssign ==. user ^. UserId)
        where_
            (   not_ $ user ^. UserName ==. val name
            )
        return (task)

setDot :: ConnectionPool -> Key User -> Text -> IO ()
setDot pool uKey unit = flip runSqlPool pool $ do
    update $ \user -> do
        set
            user [UserDefaultTimeScale =. val unit]
        where_
            (   user ^. UserId ==. val uKey
            )

insUnqPerm :: ConnectionPool -> Permission -> IO (Maybe (Key Permission))
insUnqPerm pool perm = flip runSqlPool pool $ do
    insertUnique perm

setAllowView :: ConnectionPool -> Key User -> Key User -> IO ()
setAllowView pool sbj obj = flip runSqlPool pool $ do
    update $ \permission -> do
        set
            permission [PermissionView =. val True]
        where_
            (   permission ^. PermissionSubject ==. val sbj
            &&. permission ^. PermissionObject ==. val obj
            )

setAllowEdit :: ConnectionPool -> Key User -> Key User -> IO ()
setAllowEdit pool sbj obj = flip runSqlPool pool $ do
    update $ \permission -> do
        set
            permission [ PermissionView =. val True
                       , PermissionEdit =. val True]
        where_
            (   permission ^. PermissionSubject ==. val sbj
            &&. permission ^. PermissionObject ==. val obj
            )

setBanView :: ConnectionPool -> Key User -> Key User -> IO ()
setBanView pool sbj obj = flip runSqlPool pool $ do
    update $ \permission -> do
        set
            permission [ PermissionView =. val False
                       , PermissionEdit =. val False]
        where_
            (   permission ^. PermissionSubject ==. val sbj
            &&. permission ^. PermissionObject ==. val obj
            )

setBanEdit :: ConnectionPool -> Key User -> Key User -> IO ()
setBanEdit pool sbj obj = flip runSqlPool pool $ do
    update $ \permission -> do
        set
            permission [PermissionEdit =. val False]
        where_
            (   permission ^. PermissionSubject ==. val sbj
            &&. permission ^. PermissionObject ==. val obj
            )

-- upsAllowView :: ConnectionPool -> Permission -> IO (Entity Permission)
-- upsAllowView pool perm = flip runSqlPool pool $ do
--     upsert perm [PermissionView =. val True])

-- upsAllowEdit :: ConnectionPool -> Permission -> IO (Entity Permission)
-- upsAllowEdit pool perm = flip runSqlPool pool $ do
--     upsert perm [ PermissionView =. val True
--                 , PermissionEdit =. val True ]

-- upsBanView :: ConnectionPool -> Permission -> IO (Entity Permission)
-- upsBanView pool perm = flip runSqlPool pool $ do
--     upsert perm [ PermissionView =. val False
--                 , PermissionEdit =. val False ]

-- upsBanEdit :: ConnectionPool -> Permission -> IO (Entity Permission)
-- upsBanEdit pool perm = flip runSqlPool pool $ do
--     upsert perm [PermissionEdit =. val False]

getUserByName :: ConnectionPool -> Text -> IO [Entity User]
getUserByName pool name = flip runSqlPool pool $ do
    select $ from $ \user -> do
        where_
            (   user ^. UserName ==. val name
            )
        return (user)

getEditPerm :: ConnectionPool -> Key User -> Key User -> IO [Entity Permission]
getEditPerm pool sbj obj = flip runSqlPool pool $ do
    select $ from $ \permission -> do
        where_
            (   permission ^. PermissionSubject ==. val sbj
            &&. permission ^. PermissionObject ==. val obj
            &&. permission ^. PermissionEdit ==. val True
            )
        return (permission)

getViewPerm :: ConnectionPool -> Key User -> Key User -> IO [Entity Permission]
getViewPerm pool sbj obj = flip runSqlPool pool $ do
    select $ from $ \permission -> do
        where_
            (   permission ^. PermissionSubject ==. val sbj
            &&. permission ^. PermissionObject ==. val obj
            &&. permission ^. PermissionView ==. val True
            )
        return (permission)

getPathsByTasks :: ConnectionPool -> [Key Task] -> IO [Entity Path]
getPathsByTasks pool tids = flip runSqlPool pool $ do
    select $ from $ \path -> do
        where_
            (   path ^. PathTerminal `in_` valList tids
            &&. path ^. PathInitial  `in_` valList tids
            )
        return (path)

getAllPaths :: ConnectionPool -> IO [Entity Path]
getAllPaths pool = flip runSqlPool pool $ do
    select $ from $ \path -> do
        return (path)
