{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HMapL
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Pool as Pool
import qualified Web.Scotty.Trans as ScottyT
import qualified Options.Applicative as Opts
import qualified Database.Persist as Persist
import qualified Database.Persist.TH as Persist
import qualified Database.Persist.Sqlite as Persist
import qualified Network.HTTP.Types as HTTP
import Control.Monad (unless, mzero)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (isJust)
import Data.Char (isDigit)
import Data.Monoid ((<>))
import Control.Arrow ((***), (&&&), second)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Logger (NoLoggingT(..), runNoLoggingT)
import Network.Wai.Middleware.RequestLogger (logStdout)
import GHC.Int (Int64)
import Safe (readMay)

--
--
-- Contact
--
--

type AttributeKey = T.Text
type AttributeValue = T.Text
type Email = AttributeKey
type PhoneNumber = AttributeKey

Persist.share [Persist.mkPersist Persist.sqlSettings, Persist.mkMigrate "migrateContacts"] [Persist.persistLowerCase|
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

isValidContact :: Contact -> Bool
isValidContact contact = isJust $ do
    email <- contactLookup "email" contact
    phoneNumber <- contactLookup "phoneNumber" contact
    unless (isEmail email) Nothing
    unless (isPhoneNumber phoneNumber) Nothing

-- |Username, domain, and TLD each must have minimium length of 1. (Username@Domain.TLD)
-- |Username cannot start with a digit.
isEmail :: Email -> Bool
isEmail e = let
    (username, rest) = T.span (/= '@') e
    (tld, domain) = T.span (/= '.') (T.reverse rest)
    in and [
        T.length username > 0,
        not $ isDigit $ T.head username, 
        T.length domain > 2, -- one for '@' and one for '.'
        T.length tld > 0
        ]

-- |Needs at least 10 digits to pass. All other characters are ignored.
isPhoneNumber :: PhoneNumber -> Bool
isPhoneNumber pn = let
    digitCount = sum $ map (\c -> if isDigit c then 1 else 0) (T.unpack pn)
    in 10 >= digitCount

invalidContactFailureMessage :: T.Text
invalidContactFailureMessage = "JSON data's \"email\" and/or \"phoneNumber\" is either missing or ill-formatted."

--
--
-- Server
--
--

type AppT = ReaderT App
type AppIO = AppT IO
type ScottyApp = ScottyT.ScottyT TL.Text AppIO
type ActionApp = ScottyT.ActionT TL.Text AppIO

data App = App {
    connPool :: Persist.ConnectionPool
}

serve 
    :: FilePath -- |Database path
    -> Int -- |Port number
    -> IO ()
serve fp port = runNoLoggingT $ do
    Persist.withSqlitePool (T.pack fp) port $ \pool -> NoLoggingT $ do
        let app = App pool
        Pool.withResource pool (runReaderT $ Persist.runMigration migrateContacts)
        ScottyT.scottyT port (flip runReaderT app) $ do
            ScottyT.middleware logStdout
            runRolodex

runRolodex :: ScottyApp ()
runRolodex = sequence_ $ [
    getContacts,
    getContactsById,
    postContact,
    putContact,
    deleteContact
    ]

-- Route handlers
getContacts, getContactsById, postContact, putContact, deleteContact :: ScottyApp ()

getContacts = ScottyT.get "/contacts" $ do
    params <- paramsStrict
    entities <- runDB $ Persist.selectList (makeContactQueryFilters params) []
    let pairs = map (getEntityContactIdVal &&& Persist.entityVal) entities
    jsonSuccess (filter (\pair -> contactAttributesQueryTests params (snd pair)) pairs) ""

getContactsById = ScottyT.get "/contacts/:id" $ do
    params <- paramsStrictWithIgnore ["id"]
    withContactId $ \cid -> do
        mcontact <- runDB $ Persist.get cid
        maybe
            (jsonFailure "id doesn't exist")
            (\contact ->
                if contactAttributesQueryTests params contact
                then jsonSuccess contact ""
                else jsonSuccess JSON.Null "filtered out")
            mcontact

postContact = ScottyT.post "/contacts" $ do
    body <- ScottyT.body
    liftIO $ print body
    withJSONData $ \contact -> do
        if isValidContact contact
        then do
            ckey <- runDB $ Persist.insert contact
            let cid = getContactIdVal ckey
            jsonSuccess (JSON.toJSON cid) "created"
        else jsonFailure invalidContactFailureMessage

putContact = ScottyT.put "/contacts/:id" $ do
    body <- ScottyT.body
    liftIO $ print body
    withContactId $ \cid ->
        withJSONData $ \contact -> do
            if isValidContact contact
            then do
                runDB $ Persist.repsert cid contact
                jsonSuccess JSON.Null "updated"
            else jsonFailure invalidContactFailureMessage

deleteContact = ScottyT.delete "/contacts/:id" $ do
    withContactId $ \cid -> do
        runDB $ Persist.delete cid
        jsonSuccess JSON.Null "deleted"

-- Aux functions
runDB :: Persist.SqlPersistT AppIO b -> ActionApp b
runDB f = lift $ do
    pool <- asks connPool
    Persist.runSqlPool f pool

withContactId :: (ContactId -> ActionApp ()) -> ActionApp ()
withContactId onSuccess = do
    eCid <- ScottyT.parseParam <$> ScottyT.param "id"
    either
        (const $ jsonFailure "unable to parse id")
        (onSuccess . ContactKey . Persist.SqlBackendKey)
        eCid

withJSONData :: JSON.FromJSON a => (a -> ActionApp ()) -> ActionApp ()
withJSONData f = ScottyT.rescue (ScottyT.jsonData >>= f) $ (const $ ScottyT.status HTTP.status400)

getEntityContactIdVal :: Persist.Entity Contact -> Int64
getEntityContactIdVal = getContactIdVal . Persist.entityKey

getContactIdVal :: ContactId -> Int64
getContactIdVal = Persist.unSqlBackendKey . unContactKey

makeContactQueryFilters :: [(AttributeKey, AttributeValue)] -> [Persist.Filter Contact]
makeContactQueryFilters atrs = let
    f ("email",v) fs = (ContactEmail Persist.==. v) : fs
    f ("phoneNumber",v) fs = (ContactPhoneNumber Persist.==. v) : fs
    f _ fs = fs
    in foldr f [] atrs

contactAttributesQueryTests :: [(AttributeKey, AttributeValue)] -> Contact -> Bool
contactAttributesQueryTests tests contact = let
    atrs = contactAttributes contact
    in and [ Map.lookup k atrs == Just v | (k,v) <- tests ]

paramsStrict :: ActionApp [(T.Text, T.Text)]
paramsStrict = map (TL.toStrict *** TL.toStrict) <$> ScottyT.params

paramsStrictWithIgnore
    :: [T.Text] -- |Ignore these params
    -> ActionApp [(T.Text, T.Text)]
paramsStrictWithIgnore ignoreList = do
    params <- paramsStrict
    return $ filter (\v -> not $ elem (fst v) ignoreList) params

jsonSuccess
    :: JSON.ToJSON a
    => a -- |Data
    -> T.Text -- |Message
    -> ActionApp ()
jsonSuccess a msg = ScottyT.json $ JSON.object [
    "status" JSON..= JSON.String "success",
    "data" JSON..= a,
    "message" JSON..= JSON.String msg
    ]

jsonFailure
    :: T.Text -- |Message
    -> ActionApp ()
jsonFailure msg = ScottyT.json $ JSON.object [
    "status" JSON..= JSON.String "failure",
    "data" JSON..= JSON.Null,
    "message" JSON..= JSON.String msg
    ]

---
---
--- Execute
---
---

parseArgs :: Opts.Parser (Int, Maybe FilePath) -- |(Port, Database path) 
parseArgs = let
    port =
        (Opts.option Opts.auto $ Opts.long "port" <> Opts.help "Port number. Default is 8000.")
        Opts.<|> pure 8000
    dbPath =
        (fmap Just . Opts.strOption $ Opts.long "db" <> Opts.help "SQLite3 database path")
        Opts.<|> pure Nothing
    in (,) <$> port <*> dbPath

main :: IO ()
main = do
    (port, mdbPath) <- Opts.execParser (Opts.info parseArgs Opts.idm)
    dbPath <- maybe
        (do putStrLn "No database specified. Using virtual database."
            return ":memory:")
        return
        mdbPath
    serve dbPath port
