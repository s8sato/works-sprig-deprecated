{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- for runSqlite
{-# LANGUAGE TypeApplications #-}

module Db where

import Database.Persist.TH
-- import Ch11.Gender (Gender (..))
-- for runSqlite
import Database.Persist.Postgresql
import Control.Monad.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country
    name        String
    canWeSend   Bool default=True
    UniqueCountryName name
    deriving Show
Client
    firstName   String
    lastName    String
    address     String
    country     CountryId
    age         Int Maybe
    gender      Gender Maybe
    UniqueClient firstName lastName address country
    deriving Show
Product
    name        String
    description String
    price       Double
    number      Int
    deriving Show
Purchase
    client  ClientId
    product ProductId
    number  Int
    amount  Double
    deriving Show
|]

mig = runSqlite @IO @SqlBackend "example.db" $ runMigration migrateAll

-- ins = runSqlite @IO @SqlBackend "example.db" $ do
--     spain       <- insert $ Country "Spain"
--     _client1    <- insert $ Client "Alejandro" "Serrano" "Hometown, 1" spain (Just 30) (Just Male)
--     return ()
