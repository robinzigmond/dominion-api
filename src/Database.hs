{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Database where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Postgresql
import Database.Persist.TH
import System.Environment (getEnv, lookupEnv)

import SubsidiaryTypes


data DbConnection = DbConnection {
    dbHost :: ByteString,
    dbName :: ByteString,
    dbUser :: ByteString,
    dbPassword :: ByteString,
    port :: ByteString
}


readEnv :: Text -> IO ByteString
readEnv = fmap (B.pack) . getEnv . unpack


dbConnection :: IO DbConnection
dbConnection = DbConnection <$> readEnv "dominionDbHost" <*> readEnv "dominionDbName"
    <*> readEnv "dominionDbUser" <*> readEnv "dominionDbPassword" <*> readEnv "dominionDbPort"


buildConnString :: DbConnection -> ByteString
buildConnString (DbConnection host name user pw port) =
    "host=" <> host <> " dbname=" <> name <> " user="
        <> user <> " password=" <> pw <> " port=" <> port 


isProd :: IO Bool
-- determine whether we're in the production or development environment,
-- as determined by an environment variable
isProd = fmap (/= Nothing) $ lookupEnv "DOMINIONAPIPROD"


dbConn :: IO ByteString
dbConn = do
    prod <- isProd
    if prod
        then readEnv "DATABASE_URL"
        else buildConnString <$> dbConnection


type RunDB a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a


runLiveDB :: RunDB a
runLiveDB ma = do
    connStr <- dbConn
    runNoLoggingT . withPostgresqlPool connStr 10 $ liftIO . runSqlPersistMPool ma



-- use (separate) Postgres database for running tests, because SQLite does not recognise Full Outer Joines
runTestDB :: RunDB a
runTestDB ma = do
    connStr <- buildConnString <$> (DbConnection <$> readEnv "dominionDbHost" <*> pure "dominiontest"
        <*> readEnv "dominionDbUser" <*> readEnv "dominionDbPassword" <*> readEnv "dominionDbPort")
    runNoLoggingT . withPostgresqlPool connStr 10 $ liftIO . runSqlPersistMPool ma


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Card
    name Text
    set Set
    coinCost Int Maybe
    potionCost Bool Maybe
    debtCost Int Maybe
    mainText Text
    otherText Text Maybe
    isKingdom Bool
    nonTerminal CanDoIt Maybe
    givesExtraActions CanDoIt Maybe
    returnsCard CanDoIt Maybe
    increasesHandSize CanDoIt Maybe
    extraBuy CanDoIt Maybe
    trashes Bool
    deriving Eq
    deriving Show
Type json
    name CardType
TypeCard
    cardId CardId
    typeId TypeId
    UniqueTypeCard cardId typeId
LinkPairs
    cardOne CardId
    cardTwo CardId
    UniqueLinkPairs cardOne cardTwo
|]
