{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Entity where

import Database.Persist
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
import Database.Persist.TH          ( mkMigrate
                                    , mkPersist
                                    , persistLowerCase
                                    , share
                                    , sqlSettings
                                    )
import Data.Text                    ( Text )
import Data.Time                    ( UTCTime
                                    , TimeOfDay )
import Data.Yaml.Config             ( loadYamlSettings
                                    , useEnv
                                    )

pgConf :: IO PostgresConf
pgConf = loadYamlSettings ["pgconf.yaml"] [] useEnv

pgPool :: IO ConnectionPool
pgPool = do
    conf <- pgConf
    runStdoutLoggingT $ createPostgresqlPool (pgConnStr conf) (pgPoolSize conf)

doMigration :: Migration -> IO ()
doMigration action = do
    conf <- pgConf
    runStdoutLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration action

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name                Text
    admin               Bool
    timeZone            Int
    isLazy              Bool
    resolutionMin       Int
    defaultDpy          Text Maybe
    lookUp              Int Maybe
    lookDown            Int Maybe
    encrypted           Text Maybe
    UniqueUser          name
    deriving Show
Task json
    terminal            Int
    initial             Int
    isDummy             Bool
    isDone              Bool
    isStarred           Bool
    link                Text Maybe
    startable           UTCTime Maybe
    deadline            UTCTime Maybe
    weight              Double Maybe
    title               Text Maybe
    user                UserId
    UniqueTask          terminal initial
    deriving Show Eq
Organization
    parent              UserId
    child               UserId
    UniqueOrganization  parent child
    deriving Show
Permission
    subject             UserId
    object              UserId
    view                Bool
    edit                Bool
    UniquePermission    subject object
    deriving Show
Duration
    left                TimeOfDay
    right               TimeOfDay
    user                UserId
    UniqueDuration left right user
    deriving Show
Schedule
    begin               UTCTime
    end                 UTCTime
    task                TaskId
    UniqueSchedule begin end task
    deriving Show Eq
|]

migrateDb :: IO ()
migrateDb = doMigration migrateAll
