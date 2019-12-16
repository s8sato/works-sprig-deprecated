{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE QuasiQuotes                #-}

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

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    age Int
    UniqueUserName name
    deriving Eq Show Generic
|]

getUsers :: ConnectionPool -> IO [Entity User]
getUsers pool = flip runSqlPool pool $ selectList [] []

getUser :: ConnectionPool -> Key User -> IO (Maybe (Entity User))
getUser pool = flip runSqlPool pool . getEntity

insertUser :: ConnectionPool -> User -> IO (Maybe (Entity User))
insertUser pool user = flip runSqlPool pool $ do
    mInDb <- getBy $ UniqueUserName $ user^.userName
    case mInDb of
      Just inDb -> pure Nothing
      Nothing   -> do
                     key <- insert user
                     pure $ Just $ Entity key user

migrateDb :: IO ()
migrateDb = doMigration migrateAll


--

