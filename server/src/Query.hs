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



insUser :: User -> IO ()
insUser user = do
    pool <- pgPool
    flip runSqlPool pool $ do
        insert $ user
        return ()

insDur :: Duration -> IO ()
insDur duration = do
    pool <- pgPool
    flip runSqlPool pool $ do
        insert $ duration
        return ()

insTasks :: [Task] -> IO ()
insTasks ts = do
    pool <- pgPool
    flip runSqlPool pool $ do
        sequence_ . map insert $ ts

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

getBeforeMeByTask :: ConnectionPool -> Key Task -> IO [Entity Task]
getBeforeMeByTask pool me = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        let myInitial = sub_select $ from $ \task -> do
                where_ 
                    (   task ^. TaskId ==. val me
                    )
                return $ task ^. TaskInitial
        where_
            (   task ^. TaskTerminal ==. myInitial
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
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        let myTerminal = sub_select $ from $ \task -> do
                where_ 
                    (   task ^. TaskId ==. val me
                    )
                return $ task ^. TaskTerminal
        where_
            (   task ^. TaskInitial ==. myTerminal
            )
        groupBy (task ^. TaskId)
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getMaxNode' :: ConnectionPool -> IO [(Value (Maybe Int), Value (Maybe Int))]
getMaxNode' pool = flip runSqlPool pool $ do
    select $ from $ \task -> do
        return $ (max_ (task ^. TaskTerminal), max_ (task ^. TaskInitial))

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

getTaskAssign :: ConnectionPool -> Key Task -> IO [(Entity Task, Value Text)]
getTaskAssign  pool tKey = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on  (task ^. TaskUser ==. user ^. UserId)
        where_
            (   task ^. TaskId ==. val tKey
            )
        return (task, user ^. UserName)

getDoneTasksByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getDoneTasksByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val uKey
            &&. task ^. TaskIsDone
            )
        groupBy (task ^. TaskId)
        orderBy
            [ desc (task ^. TaskIsStarred)
            , desc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getUndoneTasksByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getUndoneTasksByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val uKey
            &&. not_ (task ^. TaskIsDone)
            )
        groupBy (task ^. TaskId)
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getUndoneTrunksByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getUndoneTrunksByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val uKey
            &&. (
                notExists $ from $ \self ->
                where_
                    (   self ^. TaskInitial ==. task ^. TaskTerminal
                    &&. not_ ( task ^. TaskIsDone )
                    )
                )
            )
        groupBy (task ^. TaskId)
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getUndoneBudsByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getUndoneBudsByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val uKey
            &&. (
                notExists $ from $ \self ->
                where_
                    (   self ^. TaskTerminal ==. task ^. TaskInitial
                    &&. not_ ( task ^. TaskIsDone )
                    )
                )
            )
        groupBy (task ^. TaskId)
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getAllTrunkNode :: ConnectionPool -> IO [Value Int]
getAllTrunkNode pool = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   notExists $ from $ \self ->
                where_
                    (   self ^. TaskInitial ==. task ^. TaskTerminal
                    )
            )
        return (task ^. TaskTerminal)

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

setTaskUser :: ConnectionPool -> Key Task -> UserId -> IO ()
setTaskUser pool tKey  u = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskUser =. val (u)]
        where_ 
            (   task ^. TaskId ==. val tKey
            )

getUndoneOwnTasksByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getUndoneOwnTasksByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskUser ==. val uKey
            &&. not_ ( task ^. TaskIsDone )
            )
        return (task)

getTaskAssignByTask :: ConnectionPool -> Key Task -> IO [(Entity Task, Value Text)]
getTaskAssignByTask pool tid = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on  (task ^. TaskUser ==. user ^. UserId)
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

getUndoneNonDummyTasksByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getUndoneNonDummyTasksByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val uKey
            &&. not_ ( task ^. TaskIsDone )
            &&. not_ ( task ^. TaskIsDummy )
            )
        groupBy
            (task ^. TaskId) 
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getDoneNonDummyTasksByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getDoneNonDummyTasksByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val uKey
            &&. task ^. TaskIsDone
            &&. not_ ( task ^. TaskIsDummy )
            )
        groupBy
            (task ^. TaskId) 
        orderBy
            [ desc (task ^. TaskIsStarred)
            , desc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

delSchedulesByUser :: ConnectionPool -> Key User -> IO ()
delSchedulesByUser pool uKey = flip runSqlPool pool $ do
    delete $ from $ \schedule ->
        where_
            (   exists $ from $ \(task `InnerJoin` user) -> do
                    on  (task ^. TaskUser ==. user ^. UserId)
                    where_
                        (   task ^. TaskUser ==. val uKey
                        &&. task ^. TaskId ==. schedule ^. ScheduleTask
                        )
            )
