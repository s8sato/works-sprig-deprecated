{-# LANGUAGE OverloadedStrings #-}

module Query where
import Db                           ( pgPool
                                    , User (..)
                                    , Task (..)
                                    )
import Database.Esqueleto
import Database.Persist.Sql         ( ConnectionPool
                                    , runSqlPool
                                    , Entity (..)
                                    , Key (..)
                                    , insert
                                    , getEntity
                                    , selectList
                                    )
import Data.Time


getTasks :: ConnectionPool -> IO [Entity Task]
getTasks pool = flip runSqlPool pool $ selectList [] []

getTask :: ConnectionPool -> Key Task -> IO (Maybe (Entity Task))
getTask pool = flip runSqlPool pool . getEntity

-- insertUser :: ConnectionPool -> User -> IO (Maybe (Entity User))
-- insertUser pool user = flip runSqlPool pool $ do
--     mInDb <- getBy $ UniqueUserName $ user^.userName
--     case mInDb of
--       Just inDb -> pure Nothing
--       Nothing   -> do
--                      key <- insert user
--                      pure $ Just $ Entity key user



--

-- getTasks :: ConnectionPool -> IO [Entity User]
-- getTasks pool = flip runSqlPool pool $ do
--     select $
--     from $ \ (client `LeftOuterJoin` purchase) -> do
--     on (client ^. ClientId ==. purchase.PurchaseClient)
--     groupBy (client ^. ClientId)
--     let s = sum_ (purchase.PurchaseAmount)
--     return (client, s)

insTask :: Task -> IO ()
insTask task = do
    pool <- pgPool
    flip runSqlPool pool $ do
        insert $ task
        return ()

insUser :: User -> IO ()
insUser user = do
    pool <- pgPool
    flip runSqlPool pool $ do
        insert $ user
        return ()

ins = do
    pool <- pgPool
    flip runSqlPool pool $ do
        insert $ Task 7 8 False True (Just "https://") Nothing Nothing (Just 30) "title" 1
ins2 = do
    pool <- pgPool
    s <- zonedTimeToUTC <$> getZonedTime
    d <- addUTCTime (3 * nominalDay) . zonedTimeToUTC <$> getZonedTime
    flip runSqlPool pool $ do
        insert $ Task 8 9 True False (Just "https://eigens-pace.slack.com/archives/DJA4ZE9FH/p1576614942000200") (Just s) (Just d) (Just 120.14159) "click here" 1
ins3 = do
    pool <- pgPool
    s <- addUTCTime (3 * nominalDay) . zonedTimeToUTC <$> getZonedTime
    d <- addUTCTime (7 * nominalDay) . zonedTimeToUTC <$> getZonedTime
    flip runSqlPool pool $ do
        insert $ Task 8 9 True True (Just "https://eigens-pace.slack.com/archives/DJA4ZE9FH/p1576614942000200") (Just s) (Just d) (Just 240.14159) "あ" 1
ins4 = do
    pool <- pgPool
    s <- addUTCTime (3 * nominalDay) . zonedTimeToUTC <$> getZonedTime
    d <- addUTCTime (7 * nominalDay) . zonedTimeToUTC <$> getZonedTime
    flip runSqlPool pool $ do
        insert $ Task 9 10 True True (Just "https://eigens-pace.slack.com/archives/DJA4ZE9FH/p1576614942000200") Nothing (Just d) (Just 119.1415914159) "色" 1
ins5 = do
    pool <- pgPool
    s <- addUTCTime (3 * nominalDay) . zonedTimeToUTC <$> getZonedTime
    d <- addUTCTime (7 * nominalDay) . zonedTimeToUTC <$> getZonedTime
    flip runSqlPool pool $ do
        insert $ Task 9 11 True True (Just "https://eigens-pace.slack.com/archives/DJA4ZE9FH/p1576614942000200") (Just s) Nothing (Just 239.0001) "空" 1

insTasks :: [Task] -> IO ()
insTasks ts = do
    pool <- pgPool
    flip runSqlPool pool $ do
        sequence_ . map insert $ ts


getUndoneTasks :: ConnectionPool -> Int -> IO [Entity Task]
getUndoneTasks pool id = flip runSqlPool pool $ do
    select $ from $ \ (task) -> do
        -- E.where_ (E.not_ task E.^. IsDone)
        -- E.where_ (task E.^. weight E.==. E.val id)
        return (task)


get = do
    pool <- pgPool
    getUndoneTasks pool 1

rawTask :: IO [Task]
rawTask = do
    pool <- pgPool
    tasks <- getTasks pool
    return (map (\(Entity k v) -> v) tasks)
