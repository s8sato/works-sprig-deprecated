{-# LANGUAGE OverloadedStrings #-}

module Ch10.Builder where
    
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Data.Text
import Data.Conduit
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Text as T

data Person     = Person { firstName :: String, lastName :: String }
                deriving (Show, Eq, Ord)
data Client i   = GovOrg { clientId :: i, clientString :: String }
                | Company { clientId :: i, clientString :: String
                          , person :: Person, duty :: String }
                | Individual { clientId :: i, person :: Person }
                deriving (Show, Eq, Ord)

clientToText :: Client  Int -> B.Builder
clientToText (GovOrg i n) =
    "client(gov,"   <> B.decimal i <> B.singleton ','
                    <> B.fromText (escapeString n) <> B.singleton ')'
clientToText (Company i n p d) =
    "client(com,"   <> B.decimal i <> B.singleton ','
                    <> B.fromText (escapeString n) <> B.singleton ','
                    <> personToText p <> B.singleton ','
                    <> B.fromText (escapeString d) <> B.singleton ')'
clientToText (Individual i p) =
    "client(ind,"   <> B.decimal i <> B.singleton ','
                    <> personToText p <> B.singleton ')'

personToText :: Person -> B.Builder
personToText (Person f l) =
    "person("   <> B.fromText (escapeString f) <> B.singleton ','
                <> B.fromText (escapeString l) <> B.singleton ')'

escapeString :: String -> Text
escapeString =  replace "\n" "\\n" . replace "," "\\," .
                replace "(" "\\("  . replace ")" "\\)" . pack

saveClients :: FilePath -> [Client Int] -> IO ()
saveClients fpath clients = runConduitRes $
    L.sourceList clients .| L.map clientToText
        .| L.map (LT.toStrict . B.toLazyText)
        .| L.concatMap (\x -> [x, "\n"])
        .| T.encode T.utf8 .| B.sinkFile fpath
