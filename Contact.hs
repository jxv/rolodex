{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
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
