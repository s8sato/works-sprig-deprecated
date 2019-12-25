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
import Data.Time                    ( UTCTime )
-- import Data.Int                     ( Int64 )


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
User
    name                Text
    admin               Bool
    timeZone            Int
    defaultDpy          Int Maybe
    lookUp              Int Maybe
    lookDown            Int Maybe
    -- salt                Text
    encrypted           Text Maybe
    UniqueUser          name
    deriving Show
Task json
    terminal            Int
    initial             Int
    -- isDummy             Bool
    isDone              Bool
    isStarred           Bool
    link                Text Maybe
    start               UTCTime Maybe
    deadline            UTCTime Maybe
    weight              Double Maybe
    title               Text Maybe
    user                UserId
    UniqueTask          terminal initial
    deriving Show
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
|]

migrateDb :: IO ()
migrateDb = doMigration migrateAll

