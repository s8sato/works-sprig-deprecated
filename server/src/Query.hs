{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


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



insUser :: User -> IO (Key User)
insUser user = do
    pool <- pgPool
    flip runSqlPool pool $ do
        insert $ user

insDur :: Duration -> IO (Key Duration)
insDur duration = do
    pool <- pgPool
    flip runSqlPool pool $ do
        insert $ duration

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
 

-- getBeforeMeByTask :: ConnectionPool -> Key Task -> IO [Entity Task]
-- getBeforeMeByTask pool me = flip runSqlPool pool $ do
--     select $ from $ \(task `LeftOuterJoin` schedule) -> do
--         on  (task ^. TaskId ==. schedule ^. ScheduleTask)
--         let mMyInitial = subSelect $ from $ \task -> do
--                 where_ 
--                     (   task ^. TaskId ==. val me
--                     )
--                 return $ task ^. TaskInitial
--         case mMyInitial of
--             Just myInitial -> do
--                 where_
--                     (   task ^. TaskTerminal ==. myInitial
--                     )
--                 groupBy (task ^. TaskId)
--                 orderBy
--                     [ desc (task ^. TaskIsStarred)
--                     , asc (min_ (schedule ^. ScheduleBegin))
--                     ]
--                 return (task)
--             _ ->
--                 return [] 

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


-- getAfterMeByTask :: ConnectionPool -> Key Task -> IO [Entity Task]
-- getAfterMeByTask pool me = flip runSqlPool pool $ do
--     select $ from $ \(task `LeftOuterJoin` schedule) -> do
--         on  (task ^. TaskId ==. schedule ^. ScheduleTask)
--         let mMyTerminal = subSelect $ from $ \task -> do
--                 where_ 
--                     (   task ^. TaskId ==. val me
--                     )
--                 return $ task ^. TaskTerminal
--         case mMyTerminal of
--             Just myTerminal -> do
--                 where_
--                     (   task ^. TaskInitial ==. myTerminal
--                     )
--                 groupBy (task ^. TaskId)
--                 orderBy
--                     [ desc (task ^. TaskIsStarred)
--                     , asc (min_ (schedule ^. ScheduleBegin))
--                     ]
--                 return (task)
--             _ ->
--                 return []

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

getTasksByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getTasksByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val uKey
            )
        groupBy
            (task ^. TaskId) 
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

getNonDummyTasksByUser :: ConnectionPool -> Key User -> IO [Entity Task]
getNonDummyTasksByUser pool uKey = flip runSqlPool pool $ do
    select $ from $ \(task `LeftOuterJoin` schedule) -> do
        on  (task ^. TaskId ==. schedule ^. ScheduleTask)
        where_
            (   task ^. TaskUser ==. val uKey
            &&. not_ ( task ^. TaskIsDummy )
            )
        groupBy
            (task ^. TaskId) 
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (min_ (schedule ^. ScheduleBegin))
            ]
        return (task)

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

selLike :: ConnectionPool -> Key User -> Text -> IO [Entity Task]
selLike pool uKey l = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskUser ==. val uKey
            &&. (task ^. TaskTitle `like` just ((%) ++. val l ++. (%)))
            )
        return (task)

selStartableL :: ConnectionPool -> Key User -> UTCTime -> IO [Entity Task]
selStartableL pool uKey utc = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskUser ==. val uKey
            &&. task ^. TaskStartable >. just (val utc)
            )
        return (task)


setDot :: ConnectionPool -> Key User -> Text -> IO ()
setDot pool uKey unit = flip runSqlPool pool $ do
    update $ \user -> do
        set
            user [UserDefaultDpy =. val (Just unit)]
        where_
            (   user ^. UserId ==. val uKey
            )

setCare :: ConnectionPool -> Key User -> Int -> Int -> IO ()
setCare pool uKey up down = flip runSqlPool pool $ do
    update $ \user -> do
        set
            user [ UserLookUp   =. val (Just up)
                 , UserLookDown =. val (Just down)]
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

insUnqConnect :: ConnectionPool -> Organization -> IO (Maybe (Key Organization))
insUnqConnect pool organ = flip runSqlPool pool $ do
    insertUnique organ

getUserByName :: ConnectionPool -> Text -> IO [Entity User]
getUserByName pool name = flip runSqlPool pool $ do
    select $ from $ \user -> do
        where_
            (   user ^. UserName ==. val name
            )
        return (user)
