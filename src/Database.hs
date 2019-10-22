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
import Control.Monad.Logger (runStderrLoggingT, NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString (ByteString)
import Data.Text
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Postgresql
import Database.Persist.TH

import SubsidiaryTypes


dbConn :: ByteString
dbConn = "host=localhost dbname=dominion user=postgres password=elephant port=5432"


runDBActions :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runDBActions act = runStderrLoggingT . withPostgresqlPool dbConn 10
                    $ \pool -> liftIO $ flip runSqlPersistMPool pool act


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
