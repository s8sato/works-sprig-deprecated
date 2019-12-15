{-# LANGUAge OverloadedStrings #-}
{-# LANGUAge FlexibleInstances #-}

module Ch10.JSON where

import Data.Aeson
import Data.Text
import Ch10.Builder (Person(..), Client(..))
-- for jsonTo_
import Data.Aeson.Types
import Control.Applicative
-- for jsonToClient
import qualified Data.HashMap.Strict as M


clientToJSON :: Client Integer -> Value
clientToJSON (GovOrg i n) =
    object  [ "type"    .= String "govorg"
            , "id"      .= Number (fromInteger i)
            , "name"    .= String (pack n)
            ]
clientToJSON (Company i n p d) =
    object  [ "type"    .= String "company"
            , "id"      .= Number (fromInteger i)
            , "name"    .= String (pack n)
            , "person"  .= personToJSON p
            , "duty"    .= String (pack d)
            ]
clientToJSON (Individual i p) =
    object  [ "type"    .= String "individual"
            , "id"      .= Number (fromInteger i)
            , "person"  .= personToJSON p
            ]
personToJSON :: Person -> Value
personToJSON (Person f l) =
    object  [ "first"   .= String (pack f)
            , "last"    .= String (pack l)
            ]

jsonToPerson :: Value -> Parser Person
jsonToPerson (Object o) = Person    <$> o .: "first"
                                    <*> o .: "last"
jsonToPerson _          = Control.Applicative.empty

instance ToJSON Person where
    toJSON = personToJSON
instance FromJSON Person where
    parseJSON = jsonToPerson

jsonToClient :: FromJSON i => Value -> Parser (Client i)
jsonToClient (Object o) =
    case M.lookup "type" o of
        Just (String "govorg")      -> GovOrg       <$> o .: "id" <*> o.: "name"
        Just (String "company")     -> Company      <$> o .: "id" <*> o.: "name"
                                                    <*> o .: "person"
                                                    <*> o .: "duty"
        Just (String "individual")  -> Individual   <$> o .: "id" <*> o.: "person"
        _                           -> Control.Applicative.empty

instance ToJSON (Client Integer) where
    toJSON = clientToJSON
instance FromJSON i => FromJSON (Client i) where
    parseJSON = jsonToClient

{-
:{
toJSON $
    Company (1 :: Integer) "Black Hole Inc."
            (Person "John" "Smith") "Traveller"
:}
:{
fromJSON $ toJSON $
    Company (1 :: Integer) "Black Hole Inc."
            (Person "John" "Smith") "Traveller"
            :: Result (Client Integer)
:}
-}
