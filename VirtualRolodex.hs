{-# LANGUAGE RankNTypes #-}
module VirtualRolodex where

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
import qualified Web.Scotty as Scotty
import qualified Options.Applicative as Opts
import Control.Monad (void, unless, mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)
import Data.Char (isDigit)
import Data.IORef (newIORef, atomicModifyIORef, writeIORef)
import Data.Monoid ((<>))
import Control.Arrow ((***), second)
import Rolodex

-- Quickly done implementation for testing
openVirtualRolodex :: IO Rolodex
openVirtualRolodex = do
    keyRef <- newIORef 0
    let genKey = atomicModifyIORef keyRef (\key -> (key + 1, key))

    tdb <- STM.newTVarIO Map.empty
    let modify f = STM.atomically $ do
            db <- STM.readTVar tdb
            -- Needs RankNTypes
            (db', val) <- f db
            STM.writeTVar tdb db'
            return val

    return Rolodex {
        rolodexCreateContact = \contact -> do
            key <- genKey
            modify $ \db -> do
                return $
                    if Map.member key db
                    then (db, Nothing)
                    else (Map.insert key contact db, Just key),

        rolodexDeleteContact = \contactId -> modify $ \db -> do
            let db' = Map.delete contactId db
            return (db', Map.size db /= Map.size db'),

        rolodexUpdateContact = \contactId contact -> modify $ \db ->
            return (Map.insert contactId contact db, ()),

        rolodexFindContactById = \contactId -> do
            db <- STM.readTVarIO tdb
            return (Map.lookup contactId db), 

        rolodexAllContacts = fmap Map.toList (STM.readTVarIO tdb),
        
        rolodexClose = do
            STM.atomically (STM.writeTVar tdb Map.empty)
            writeIORef keyRef 0
            return True
    }
