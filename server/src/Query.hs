{-# LANGUAGE OverloadedStrings #-}

module Query where
import Db                           
                                    -- ( pgPool
                                    -- , User (..)
                                    -- , Task (..)
                                    -- )
import Database.Esqueleto
import Database.Persist.Sql         ( ConnectionPool
                                    , runSqlPool
                                    , Entity (..)
                                    , Key (..)
                                    , insert
                                    -- , getEntity
                                    -- , selectList
                                    )
import Data.Time



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


getUndoneTasks :: ConnectionPool -> Int -> IO [Entity Task]
getUndoneTasks pool user = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskUser ==. val user
            &&. not_ ( task ^. TaskIsDone )
            )
        orderBy
            [ asc (task ^. TaskDeadline)
            , asc (task ^. TaskStart)
            , desc (task ^. TaskIsStarred)
            , asc (task ^. TaskTitle)
            ]
        return (task)


keyFromInt :: Integral a => a -> BackendKey SqlBackend
keyFromInt = SqlBackendKey . fromIntegral





get = do
    pool <- pgPool
    getUndoneTasks pool 1



-- rawTask :: IO [Task]
-- rawTask = do
--     pool <- pgPool
--     tasks <- getTasks pool
--     return (map (\(Entity k v) -> v) tasks)



setTasksDone :: ConnectionPool -> [Integer] -> IO ()
setTasksDone pool ids = flip runSqlPool pool $ do
    update $ \task -> do
        set
            task [TaskIsDone =. val True]
        where_ 
            (   task ^. TaskId `in_` valList (map (TaskKey . keyFromInt ) ids)
            )


done = do
    pool <- pgPool
    setTasksDone pool [6]


setStarSwitched :: ConnectionPool -> Integer -> IO ()
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
--             , asc (task ^. TaskStart)
--             , desc (task ^. TaskIsStarred)
--             , asc (task ^. TaskTitle)
--             ]
--         return (task)

getMe :: ConnectionPool -> Integer -> IO [Entity Task]
getMe pool me = flip runSqlPool pool $ do
    select $ from $ \task -> do
        where_
            (   task ^. TaskId ==. val (TaskKey . keyFromInt $ me)
            )
        return (task)

getBeforeMe :: ConnectionPool -> Integer -> IO [Entity Task]
getBeforeMe pool me = flip runSqlPool pool $ do
    select $ from $ \task -> do
        let myInitial = sub_select $ from $ \task -> do
            where_ 
                (   task ^. TaskId ==. val (TaskKey . keyFromInt $ me)
                )
            return $ task ^. TaskInitial
        where_
            (   task ^. TaskTerminal ==. myInitial
            )
        orderBy
            [ asc (task ^. TaskDeadline)
            , asc (task ^. TaskStart)
            , desc (task ^. TaskIsStarred)
            , asc (task ^. TaskTitle)
            ]
        return (task)

getAfterMe :: ConnectionPool -> Integer -> IO [Entity Task]
getAfterMe pool me = flip runSqlPool pool $ do
    select $ from $ \task -> do
        let myTerminal = sub_select $ from $ \task -> do
            where_ 
                (   task ^. TaskId ==. val (TaskKey . keyFromInt $ me)
                )
            return $ task ^. TaskTerminal
        where_
            (   task ^. TaskInitial ==. myTerminal
            )
        orderBy
            [ asc (task ^. TaskDeadline)
            , asc (task ^. TaskStart)
            , desc (task ^. TaskIsStarred)
            , asc (task ^. TaskTitle)
            ]
        return (task)
