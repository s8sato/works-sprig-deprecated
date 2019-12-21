{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.Time (UTCTime)

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
    -- isDummy         Bool
    isDone          Bool
    isStarred       Bool
    link            Text Maybe
    start           UTCTime Maybe
    deadline        UTCTime Maybe
    weight          Double Maybe
    title           Text Maybe
    user            Int
    deriving Show
|]

migrateDb :: IO ()
migrateDb = doMigration migrateAll

