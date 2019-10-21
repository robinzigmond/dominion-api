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
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Text
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH

import SubsidiaryTypes


asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader = id


dbConn :: Text
dbConn = "c:\\Users\\robin\\Documents\\code\\dominion-api\\dbtest.db"


runDBActions :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runDBActions = runSqlite dbConn . asSqlBackendReader


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
    trashes Bool
Type json
    name CardType
TypeCard
    cardId CardId
    typeId TypeId
    UniqueTypeCard cardId typeId
CardLinks
    cardId CardId
    linkedCards [CardId]
    UniqueCardLinks cardId
|]
