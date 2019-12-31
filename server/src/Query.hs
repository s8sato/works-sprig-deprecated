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

-- getTasks :: ConnectionPool -> IO [Entity Task]
-- getTasks pool = flip runSqlPool pool $ selectList [] []

-- getTask :: ConnectionPool -> Key Task -> IO (Maybe (Entity Task))
-- getTask pool = flip runSqlPool pool . getEntity

-- insertUser :: ConnectionPool -> User -> IO (Maybe (Entity User))
-- insertUser pool user = flip runSqlPool pool $ do
--     mInDb <- getBy $ UniqueUserName $ user^.userName
--     case mInDb of
--       Just inDb -> pure Nothing
--       Nothing   -> do
--                      key <- insert user
--                      pure $ Just $ Entity key user

-- getTasks :: ConnectionPool -> IO [Entity User]
-- getTasks pool = flip runSqlPool pool $ do
--     select $
--     from $ \ (client `LeftOuterJoin` purchase) -> do
--     on (client ^. ClientId ==. purchase.PurchaseClient)
--     groupBy (client ^. ClientId)
--     let s = sum_ (purchase.PurchaseAmount)
--     return (client, s)

-- insTask :: Task -> IO ()
-- insTask task = do
--     pool <- pgPool
--     flip runSqlPool pool $ do
--         insert $ task
--         return ()

insUser :: User -> IO ()
insUser user = do
    pool <- pgPool
    flip runSqlPool pool $ do
        insert $ user
        return ()

-- ins = do
--     pool <- pgPool
--     flip runSqlPool pool $ do
--         insert $ Task 7 8 False True (Just "https://") Nothing Nothing (Just 30) "title" 1

insTasks :: [Task] -> IO ()
insTasks ts = do
    pool <- pgPool
    flip runSqlPool pool $ do
        sequence_ . map insert $ ts

-- getUndoneTasks :: ConnectionPool -> Int -> IO [Entity Task]
-- getUndoneTasks pool user = flip runSqlPool pool $ do
--     select $ from $ \task -> do
--         where_
--             (   task ^. TaskUser ==. val (toSqlKey $ fromIntegral user)
--             &&. not_ ( task ^. TaskIsDone )
--             )
--         orderBy
--             [ desc (task ^. TaskIsStarred)
--             , asc (task ^. TaskDeadline)
--             , asc (task ^. TaskStartable)
--             , asc (task ^. TaskTitle)
--             ]
--         return (task)

keyFromInt :: Integral a => a -> BackendKey SqlBackend
keyFromInt = SqlBackendKey . fromIntegral

-- get = do
--     pool <- pgPool
--     getUndoneTasks pool 1

-- rawTask :: IO [Task]
-- rawTask = do
--     pool <- pgPool
--     tasks <- getTasks pool
--     return (map (\(Entity k v) -> v) tasks)

-- setTasksDone :: ConnectionPool -> [Int] -> IO ()
-- setTasksDone pool ids = flip runSqlPool pool $ do
--     update $ \task -> do
--         set
--             task [TaskIsDone =. val True]
--         where_ 
--             (   task ^. TaskId `in_` valList (map (TaskKey . keyFromInt ) ids)
--             )

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

-- getAroundTasks :: ConnectionPool -> Int -> Integer -> IO [Entity Task]
-- getAroundTasks pool user focus = flip runSqlPool pool $ do
--     select $ from $ \task -> do
--         where_
--             (   task ^. TaskUser ==. val user
--             &&. not_ ( task ^. TaskIsDone )
--             )
--         orderBy
--             [ asc (task ^. TaskDeadline)
--             , asc (task ^. TaskStartable)
--             , desc (task ^. TaskIsStarred)
--             , asc (task ^. TaskTitle)
--             ]
--         return (task)

getMe :: ConnectionPool -> Int -> IO [(Entity Task, Value Text)]
getMe pool me = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on (task ^. TaskUser ==. user ^. UserId)
        where_
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ me)
            )
        return (task, user ^. UserName)

getBeforeMe :: ConnectionPool -> Int -> IO [(Entity Task, Value Text)]
getBeforeMe pool me = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on (task ^. TaskUser ==. user ^. UserId)
        let myInitial = sub_select $ from $ \task -> do
            where_ 
                (   task ^. TaskId ==. val (TaskKey . keyFromInt $ me)
                )
            return $ task ^. TaskInitial
        where_
            (   task ^. TaskTerminal ==. myInitial
            )
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (task ^. TaskDeadline)
            , asc (task ^. TaskStartable)
            -- , asc (task ^. TaskTitle)
            ]
        return (task, user ^. UserName)

getAfterMe :: ConnectionPool -> Int -> IO [(Entity Task, Value Text)]
getAfterMe pool me = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on (task ^. TaskUser ==. user ^. UserId)
        let myTerminal = sub_select $ from $ \task -> do
            where_ 
                (   task ^. TaskId ==. val (TaskKey . keyFromInt $ me)
                )
            return $ task ^. TaskTerminal
        where_
            (   task ^. TaskInitial ==. myTerminal
            )
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (task ^. TaskDeadline)
            , asc (task ^. TaskStartable)
            -- , asc (task ^. TaskTitle)
            ]
        return (task, user ^. UserName)

-- maybeMaxTaskIdKey' :: ConnectionPool -> IO [Value (Maybe (Key Task))]
-- maybeMaxTaskIdKey' pool = flip runSqlPool pool $ do
--     select $ from $ \task -> do
--         return $ max_ (task ^. TaskId)

-- maybeMaxTaskIdKey :: IO (Maybe (Key Task))
-- maybeMaxTaskIdKey = do
--     pool <- pgPool
--     vmks <- maybeMaxTaskIdKey' pool
--     let mk = case vmks of
--                 []  -> Nothing
--                 _   -> head $ map (\(Value mk) -> mk) vmks
--     return mk

getMaxNode' :: ConnectionPool -> IO [(Value (Maybe Int), Value (Maybe Int))]
getMaxNode' pool = flip runSqlPool pool $ do
    select $ from $ \task -> do
        return $ (max_ (task ^. TaskTerminal), max_ (task ^. TaskInitial))

-- getMe' :: MonadIO m => Integer -> SqlPersistT m [Entity Task]
-- getMe' me = 
--     select $ from $ \task -> do
--         where_
--             (   task ^. TaskId ==. val (TaskKey . keyFromInt $ me)
--             )
--         return (task)

-- meh :: ConnectionPool -> Integer -> IO [Entity Task]
-- meh pool arg = 
--     flip runSqlPool pool $ do
--         getMe' arg

getUndoneTaskAssigns :: ConnectionPool -> Int -> IO [(Entity Task, Value Text)]
getUndoneTaskAssigns pool uid = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on (task ^. TaskUser ==. user ^. UserId)
        where_
            (   task ^. TaskUser ==. val (UserKey . keyFromInt $ uid)
            -- (   task ^. TaskUser ==. val (toSqlKey $ fromIntegral uid)
            &&. not_ ( task ^. TaskIsDone )
            )
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (task ^. TaskDeadline)
            , asc (task ^. TaskStartable)
            -- , asc (task ^. TaskTitle)
            ]
        return (task, user ^. UserName)

getUserById :: ConnectionPool -> Int -> IO [Entity User]
getUserById pool id = flip runSqlPool pool $ do
    select $ from $ \user -> do
        where_
            (   user ^. UserId ==. val (UserKey . keyFromInt $ id)
            )
        return (user)

-- getUserAndDurations :: ConnectionPool -> Int -> IO ([Entity User],[Entity Duration])
-- getUserById pool id = flip runSqlPool pool $ do
--     select $ from $ \user `InnerJoin` duration -> do
--         on ( user ^. UserId ==. duration ^. DurationUser )
--         where_
--             (   user ^. UserId ==. val (UserKey . keyFromInt $ id)
--             )
--         orderBy
--             [ asc (duration ^. DurationLeft)
--             ]
--         return (user, duration)

getDurationsById :: ConnectionPool -> Int -> IO [Entity Duration]
getDurationsById pool id = flip runSqlPool pool $ do
    select $ from $ \duration -> do
        where_
            (   duration ^. DurationUser ==. val (UserKey . keyFromInt $ id)
            )
        orderBy
            [ asc (duration ^. DurationLeft)
            ]
        return (duration)

getTaskAssignById :: ConnectionPool -> Int -> IO [(Entity Task, Value Text)]
getTaskAssignById  pool id = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on (task ^. TaskUser ==. user ^. UserId)
        where_
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ id)
            )
        return (task, user ^. UserName)


getArchivesAssigns :: ConnectionPool -> Int -> IO [(Entity Task, Value Text)]
getArchivesAssigns pool uid = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on (task ^. TaskUser ==. user ^. UserId)
        where_
            (   task ^. TaskUser ==. val (UserKey . keyFromInt $ uid)
            &&. task ^. TaskIsDone
            )
        orderBy
            [ desc (task ^. TaskIsStarred)
            , desc (task ^. TaskDeadline)
            , desc (task ^. TaskStartable)
            ]
        return (task, user ^. UserName)


getTrunkAssigns :: ConnectionPool -> Int -> IO [(Entity Task, Value Text)]
getTrunkAssigns pool uid = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on (task ^. TaskUser ==. user ^. UserId)
        where_
            (   task ^. TaskUser ==. val (UserKey . keyFromInt $ uid)
            &&. (
                notExists $ from $ \self ->
                where_
                    (   self ^. TaskInitial ==. task ^. TaskTerminal
                    )
                )
            )
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (task ^. TaskDeadline)
            , asc (task ^. TaskStartable)
            ]
        return (task, user ^. UserName)


getBudsAssigns :: ConnectionPool -> Int -> IO [(Entity Task, Value Text)]
getBudsAssigns pool uid = flip runSqlPool pool $ do
    select $ from $ \(task `InnerJoin` user) -> do
        on (task ^. TaskUser ==. user ^. UserId)
        where_
            (   task ^. TaskUser ==. val (UserKey . keyFromInt $ uid)
            &&. (
                notExists $ from $ \self ->
                where_
                    (   self ^. TaskTerminal ==. task ^. TaskInitial
                    )
                )
            )
        orderBy
            [ desc (task ^. TaskIsStarred)
            , asc (task ^. TaskDeadline)
            , asc (task ^. TaskStartable)
            ]
        return (task, user ^. UserName)
