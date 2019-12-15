{-# LANGUAGE OverloadedStrings #-}

module Ch10.Parsing where

import Control.Applicative
import Data.Text
import Data.Attoparsec.Text
import Ch10.Builder (Person(..), Client(..), clientToText)
-- for parseClients
import Data.Attoparsec.Combinator
-- for loadClients
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.Text as T

aChar :: Parser Char
aChar =     (const ',') <$> (string "\\,")
        <|> (const '\n') <$> (string "\\n")
        <|> (const '(') <$> (string "\\(")
        <|> (const ')') <$> (string "\\)")
        <|> satisfy (notInClass ",\n()")

aString :: Parser String
-- aString = ((:) <$> aChar <*> aString) <|> (pure "")
aString = many aChar

aPerson :: Parser Person
aPerson = Person    <$ string "person(" <*> aString
                    <* char ',' <*> aString <* char ')'

aClient :: Parser (Client Int)
aClient =   GovOrg      <$ string "client(gov," <*> decimal
                        <* char ',' <*> aString <* char ')'
        <|> Company     <$ string "client(com," <*> decimal
                        <* char ',' <*> aString <* char ','
                        <*> aPerson <* char ',' <*> aString <* char ')'
        <|> Individual  <$ string "client(ind," <*> decimal
                        <* char ',' <*> aPerson <* char ')'

parseClients :: Parser [Client Int]
parseClients = sepBy aClient (char '\n')

loadClients :: FilePath -> IO [Client Int]
loadClients fpath = runConduitRes $
    B.sourceFile fpath .| T.decode T.utf8 .| sinkParser parseClients
