{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

-- {-# LANGUAGE UnicodeSyntax #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
-- CORS
import Network.Wai.Middleware.Cors 

-- data User = User
--   { userId        :: Int
--   , userFirstName :: String
--   , userLastName  :: String
--   } deriving (Eq, Show)

yea :: Int
yea = 1
qua :: Int
qua = 4
mon :: Int
mon = 12
wee :: Int
wee = 52
day :: Int
day = 365
hou :: Int
hou = 8760
min :: Int
min = 525600
sec :: Int
sec = 31536000

data DateTime = DateTime
    { date :: String -- "YYYY/MM/DD"
    , time :: String -- "HH:MM'SS"
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''DateTime)

data Bar = Bar
    { dot :: Int
    , sharp :: Int
    , exclamation :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Bar)

data Task = Task
    { isDone :: Bool
    , isStarred :: Bool
    , title :: String
    , start :: DateTime
    , deadline :: DateTime
    , weight :: Int
    , bar :: Bar
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Task)

data Model = Model
    { scale :: Int
    , tasks :: [Task]
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Model)

type API = "model" :> Get '[JSON] Model

startApp :: IO ()
startApp = run 8080 app

app :: Application
-- app = serve api server
app = simpleCors $ serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return model

model :: Model
model = Model day
        [ Task False True "curry" (DateTime "2019/12/15" "06:35'00") (DateTime "2019/12/16" "06:36:00") 30 (Bar 12 15 16)
        , Task True False "hayasi" (DateTime "2019/12/17" "06:37'00") (DateTime "2019/12/18" "06:38:00") 60 (Bar 9 15 16)
        ]

-- 
