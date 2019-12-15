{-# LANGUAGE TemplateHaskell #-}

module Ch11.Gender where

import Database.Persist.TH

data Gender = Male | Female
    deriving (Show, Read, Eq)
derivePersistField "Gender"
