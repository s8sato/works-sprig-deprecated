{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Db where

import Control.Monad.Logger         ( runStdoutLoggingT )
import Control.Monad.Trans.Reader   ( runReaderT )
import Control.Monad.Trans.Resource ( runResourceT )
import Database.Persist.Postgresql  ( PostgresConf (..)
                                    , withPostgresqlConn
                                    , createPostgresqlPool
                                    )
import Database.Persist.Sql         ( Migration
                                    , ConnectionPool
                                    , runMigration
                                    )

import Control.Lens                 ((^.))
import Data.Text                    (Text)
import Database.Persist             
import Database.Persist.Sql         (ConnectionPool
                                   , runSqlPool
                                    )
import Database.Persist.TH          (mkMigrate
                                   , mkPersist
                                   , persistLowerCase
                                   , share
                                   , sqlSettings
                                   , mpsGenerateLenses
                                    )
import GHC.Generics
import Data.Time
import qualified Database.Esqueleto as E

import Control.Monad.IO.Class   (liftIO)
import Database.Persist.Sql
import Network.Wai.Handler.Warp (run, Port)
import Servant


pgPool :: IO ConnectionPool
pgPool = do
    -- conf <- pgConf
    let conf = PostgresConf "host=localhost port=5432 user=postgres dbname=postgres password=Pfg5NlqJ" 5
    runStdoutLoggingT $ createPostgresqlPool (pgConnStr conf) (pgPoolSize conf)

doMigration :: Migration -> IO ()
doMigration action = do
    -- conf <- pgConf
    let conf = PostgresConf "host=localhost port=5432 user=postgres dbname=postgres password=Pfg5NlqJ" 5
    runStdoutLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration action

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name        String
    UniqueUserName name
    deriving Show
Task json
    terminal        Int
    initial         Int
    isDone          Bool
    isStarred       Bool
    link            String Maybe
    start           UTCTime Maybe
    deadline        UTCTime Maybe
    weight          Double Maybe
    title           String
    user            Int
    deriving Show
|]


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

migrateDb :: IO ()
migrateDb = doMigration migrateAll


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

getUndoneTasksByUser :: ConnectionPool -> Int -> IO [Entity Task]
getUndoneTasksByUser pool id = flip runSqlPool pool $ do
    E.select $ E.from $ \ (task) -> do
        -- E.where_ (E.not_ task E.^. IsDone)
        -- E.where_ (task E.^. weight E.==. E.val id)
        return (task)


get = do
    pool <- pgPool
    getUndoneTasksByUser pool 1


-- type ApiDef  = Get '[JSON] [Entity Task]
--         :<|> "tasks" :> Get '[JSON] [Entity Task]

-- server :: ConnectionPool -> Server ApiDef
-- server pool = (liftIO $ getTasks pool)
--         :<|> (liftIO $ getTasks pool)

-- api :: Proxy ApiDef
-- api = Proxy

-- app :: ConnectionPool -> Application
-- app pool = serve api $ server pool

-- mkApp :: IO Application
-- mkApp = do
--     migrateDb
--     pool <- pgPool
--     return $ app pool

-- startServer :: Port -> IO ()
-- startServer port = do
--     putStrLn "{- ----------------------------"
--     putStrLn " - start server!"
--     putStrLn " ----------------------------- -}"
--     run port =<< mkApp

rawTask :: IO [Task]
rawTask = do
    pool <- pgPool
    tasks <- getTasks pool
    return (map (\(Entity k v) -> v) tasks)
