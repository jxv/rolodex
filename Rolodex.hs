-- For persistent
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Rolodex where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.HashMap.Lazy as HMapL
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Serialize as Serialize
import qualified Database.SQLite3 as SQLite
import qualified Database.Persist as Persist
import qualified Database.Persist.TH as Persist
import qualified Database.Persist.Sqlite as Persist
import Control.Monad (void, unless, mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)
import Data.Char (isDigit)
import Data.IORef (newIORef, atomicModifyIORef, writeIORef)
import Data.Monoid ((<>))
import Control.Arrow ((***), second)


--
--
-- Data
--
--

-- Aliases for clarity and flexibility
type ContactIdn = Int
type AttributeKey = T.Text
type AttributeValue = T.Text
type Email = AttributeKey
type PhoneNumber = AttributeKey

Persist.share [Persist.mkPersist Persist.sqlSettings, Persist.mkMigrate "migrateAll"] [Persist.persistLowerCase|
Contact
    email AttributeValue
    phoneNumber AttributeValue
    attributes (Map.Map AttributeKey AttributeValue)
    deriving Show Eq
|]

instance JSON.ToJSON Contact where
    toJSON contact = JSON.toJSON (insertReqs $ contactAttributes contact)
        where
            insertReqs = mconcat $ map (uncurry Map.insert . second ($ contact)) reqs
            reqs = [("email", contactEmail), ("phoneNumber", contactPhoneNumber)]

instance JSON.FromJSON Contact where
    parseJSON (JSON.Object o) = Contact
        <$> o JSON..: "email"
        <*> o JSON..: "phoneNumber"
        <*> (JSON.parseJSON . JSON.Object . HMapL.filterWithKey (\_ k -> notElem k reqs)) o
        where reqs = ["email","phoneNumber"]
    parseJSON _ = mzero

contactLookup :: AttributeKey -> Contact -> Maybe AttributeValue
contactLookup key contact = case key of
    "email" -> Just (contactEmail contact)
    "phoneNumber" -> Just (contactPhoneNumber contact)
    key -> Map.lookup key (contactAttributes contact)

me, me2 :: Contact
me = Contact "joevargas92@gmail.com" "3233822011" $ Map.fromList [("firstName", "Joe"), ("lastName", "Vargas")]
me2 = Contact "jxv@hush.com" "3232218067" $ Map.fromList [("firstName", "Joseph"), ("lastName", "Vargas")]

isValidContact :: Contact -> Bool
isValidContact contact = Maybe.isJust $ do
    email <- contactLookup "email" contact
    phoneNumber <- contactLookup "phoneNumber" contact
    unless (isEmail email) Nothing
    unless (isPhoneNumber phoneNumber) Nothing

-- Crude. Use Attoparsec for better results.
isEmail :: Email -> Bool
isEmail e = let
    (userName, rest) = T.span (/= '@') e
    (domain, tld) = T.span (/= '.') rest
    in and [
        T.length userName > 0,
        not $ isDigit $ T.head userName, 
        T.length domain > 1, -- one for '@'
        T.length tld > 1 -- one for '.'
        ]

-- Crude. Use Attoparsec for better results.
isPhoneNumber :: PhoneNumber -> Bool
isPhoneNumber pn = let
    digitCount = sum $ map (\c -> if isDigit c then 1 else 0) (T.unpack pn)
    in 10 >= digitCount

invalidContactFailureMessage :: T.Text
invalidContactFailureMessage = "JSON data's \"email\" and/or \"phoneNumber\" is either missing or ill-formatted."

--
--
-- Database
--
--

-- Interface
data Rolodex = Rolodex {
    rolodexCreateContact :: Contact -> IO (Maybe ContactIdn),
    rolodexDeleteContact :: ContactIdn -> IO Bool,
    rolodexUpdateContact :: ContactIdn -> Contact -> IO (),
    rolodexFindContactById :: ContactIdn -> IO (Maybe Contact),
    rolodexAllContacts :: IO [(ContactIdn, Contact)], rolodexClose :: IO Bool
}

openSQLiteRolodex :: FilePath -> IO (Maybe Rolodex)
openSQLiteRolodex fp = (flip catch) (\e -> print (e :: SomeException) >> return Nothing) $ do
    --connPool <- Persist.createSqlitePool (T.pack fp) 1
--    SQLite.exec conn "CREATE TABLE IF NOT EXISTS contacts (id INTEGER PRIMARY KEY, email TEXT, phoneNumber TEXT, attributes BLOB);"
--    let insertIntoContacts e pn atrs =
--            "INSERT INTO contacts (email, phoneNumber, attributes) VALUES('" <> e <> "','" <> pn <> "',NULL);"
    return $ Just Rolodex {
        rolodexCreateContact = \contact -> do
{-
            stmt <- SQLite.prepare conn $
                insertIntoContacts
                    (contactEmail contact)
                    (contactPhoneNumber contact)
                    (T.decodeUtf8 . Serialize.encode . map (T.encodeUtf8 *** T.encodeUtf8) . Map.toList $ contactAttributes contact)
            print stmt
-}
            return undefined,

        rolodexDeleteContact = \contactId -> do
            return undefined,

        rolodexUpdateContact = \contactId contact -> do
            return undefined,

        rolodexFindContactById = \contactId -> do
            return undefined,

        rolodexAllContacts = do
            return undefined,

        rolodexClose = do
--            SQLite.close conn
            return True
    }
