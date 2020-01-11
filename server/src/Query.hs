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

keyFromInt :: Integral a => a -> BackendKey SqlBackend
keyFromInt = SqlBackendKey . fromIntegral

setTaskDoneOrUndone :: ConnectionPool -> Int -> IO ()
setTaskDoneOrUndone pool id = flip runSqlPool pool $ do
    update $ \task -> do
        let isDone = sub_select $ from $ \task -> do
                where_ 
                    (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
                    )
                return $ task ^. TaskIsDone
        set
            task [TaskIsDone =. not_ isDone]
        where_ 
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )

setStarSwitched :: ConnectionPool -> Int -> IO ()
setStarSwitched pool id = flip runSqlPool pool $ do
    update $ \task -> do
        let isStarred = sub_select $ from $ \task -> do
                where_ 
                    (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
                    )
                return $ task ^. TaskIsStarred
        set
            task [TaskIsStarred =. not_ isStarred]
        where_ 
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )

getBeforeMeByTaskId :: ConnectionPool -> Key Task -> IO [Entity Task]
getBeforeMeByTaskId pool me = flip runSqlPool pool $ do
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

getMeByTaskId :: ConnectionPool -> Key Task -> IO [Entity Task]
getMeByTaskId pool me = flip runSqlPool pool $ do
    select $ from $ \task  -> do
        where_
            (   task ^. TaskId ==. val me
            )
        return (task)

getAfterMeByTaskId :: ConnectionPool -> Key Task -> IO [Entity Task]
getAfterMeByTaskId pool me = flip runSqlPool pool $ do
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

getUserById :: ConnectionPool -> Int -> IO [Entity User]
getUserById pool id = flip runSqlPool pool $ do
    select $ from $ \user -> do
        where_
            (   user ^. UserId ==. val (UserKey . keyFromInt $ id)
            )
        return (user)

getDurationsByUserId :: ConnectionPool -> Int -> IO [Entity Duration]
getDurationsByUserId pool uid = flip runSqlPool pool $ do
    select $ from $ \duration -> do
        where_
            (   duration ^. DurationUser ==. val (UserKey . keyFromInt $ uid)
            )
        orderBy
            [ asc (duration ^. DurationLeft)
            ]
        return (duration)

getTaskAssignById :: ConnectionPool -> Int -> IO [(Entity Task, Value Text)]
getTaskAssignById  pool id = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on  (task ^. TaskUser ==. user ^. UserId)
        where_
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )
        return (task, user ^. UserName)


getDoneTasksByUserId :: ConnectionPool -> Int -> IO [Entity Task]
getDoneTasksByUserId pool uid = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskUser ==. val (UserKey . keyFromInt $ uid)
            &&. task ^. TaskIsDone
            )
        orderBy
            [ desc (task ^. TaskIsStarred)
            , desc (task ^. TaskDeadline)
            , desc (task ^. TaskStartable)
            ]
        return (task)

getUndoneTrunksByUserId :: ConnectionPool -> Int -> IO [Entity Task]
getUndoneTrunksByUserId pool uid = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val (UserKey . keyFromInt $ uid)
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

getUndoneBudsByUserId :: ConnectionPool -> Int -> IO [Entity Task]
getUndoneBudsByUserId pool uid = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val (UserKey . keyFromInt $ uid)
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

setTaskDone :: ConnectionPool -> Int -> IO ()
setTaskDone pool id = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskIsDone =. val (True)]
        where_ 
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )

setTaskStarred :: ConnectionPool -> Int -> IO ()
setTaskStarred pool id = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskIsStarred =. val (True)]
        where_ 
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )

setTaskLink :: ConnectionPool -> Int -> Text -> IO ()
setTaskLink pool id l = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskLink =. val (Just l)]
        where_ 
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )

setTaskStartable :: ConnectionPool -> Int -> UTCTime -> IO ()
setTaskStartable pool id s = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskStartable =. val (Just s)]
        where_ 
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )

setTaskDeadline :: ConnectionPool -> Int -> UTCTime -> IO ()
setTaskDeadline pool id d = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskDeadline =. val (Just d)]
        where_ 
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )

setTaskWeight :: ConnectionPool -> Int -> Double -> IO ()
setTaskWeight pool id w = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskWeight =. val (Just w)]
        where_ 
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )

setTaskTitle :: ConnectionPool -> Int -> Text -> IO ()
setTaskTitle pool id tt = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskTitle=. val (Just tt)]
        where_ 
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )

setTaskUser :: ConnectionPool -> Int -> UserId -> IO ()
setTaskUser pool id  u = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskUser =. val (u)]
        where_ 
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )

getUndoneOwnTasksByUserId :: ConnectionPool -> Int -> IO [Entity Task]
getUndoneOwnTasksByUserId pool uid = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskUser ==. val (UserKey . keyFromInt $ uid)
            &&. not_ ( task ^. TaskIsDone )
            )
        return (task)

getTaskAssignByTaskId :: ConnectionPool -> Key Task -> IO [(Entity Task, Value Text)]
getTaskAssignByTaskId pool tid = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on  (task ^. TaskUser ==. user ^. UserId)
        where_
            (   task ^. TaskId ==. val tid
            )
        return (task, user ^. UserName)

getSchedulesByTaskId :: ConnectionPool -> Key Task -> IO [Entity Schedule]
getSchedulesByTaskId pool tid = flip runSqlPool pool $ do
    select $ from $ \schedule -> do
        where_
            (   schedule ^. ScheduleTask ==. val tid
            )
        orderBy
            [ asc (schedule ^. ScheduleBegin)
            ]
        return (schedule)

getUndoneNonDummyTasksByUserId :: ConnectionPool -> Int -> IO [Entity Task]
getUndoneNonDummyTasksByUserId pool uid = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val (UserKey . keyFromInt $ uid)
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

delSchedulesByUserId :: ConnectionPool -> Int -> IO ()
delSchedulesByUserId pool uid = flip runSqlPool pool $ do
    delete $ from $ \schedule ->
        where_
            (   exists $ from $ \(task `InnerJoin` user) -> do
                    on  (task ^. TaskUser ==. user ^. UserId)
                    where_
                        (   task ^. TaskUser ==. val (UserKey . keyFromInt $ uid)
                        &&. task ^. TaskId ==. schedule ^. ScheduleTask
                        )
            )
