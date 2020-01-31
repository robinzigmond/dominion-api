{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Auth where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe (catMaybes)
import Database.Persist
import Database.Persist.Sql (runMigration)
import Database.Persist.Postgresql (replace)
import Data.Text hiding (map)
import Servant

import Database
import Instances
import SubsidiaryTypes


authCheck :: BasicAuthCheck ()
authCheck = BasicAuthCheck $ \(BasicAuthData username password) -> do
    theUsername <- readEnv "dominionAdminUser"
    thePassword <- readEnv "dominionAdminPassword"
    return $ if username == theUsername && password == thePassword
        then Authorized ()
        else Unauthorized


authContext :: Context (BasicAuthCheck () ': '[])
authContext = authCheck :. EmptyContext


type PrivateAPI = BasicAuth "dominion" () :> (
                        "cards" :> "new" :> ReqBody '[JSON] CardWithTypesAndLinks
                            :> PostNoContent '[JSON] NoContent
                        :<|> "cards" :> "update" :> Capture "card-name" Text
                            :> ReqBody '[JSON] CardWithTypesAndLinks :> PutNoContent '[JSON] NoContent
                        :<|> "cards" :> "delete" :> Capture "card-name" Text
                            :> DeleteNoContent '[JSON] NoContent
                    )


insertCard :: RunDB () -> CardWithTypesAndLinks -> Handler ()
insertCard runDB (CardWithTypesAndLinks baseCard types links) =
    liftIO . runDB $ do
        runMigration migrateAll
        cardId <- insert baseCard
        -- insert linked cards
        forM links $ \cardName -> do
            maybeCard <- selectFirst [CardName ==. cardName] []
            case maybeCard of
                Nothing -> return () -- if no card exists with the give name, we'll just ignore it
                -- (would be better to throw an error, but this is simpler, and will only affect me
                -- so I don't have to expend too much effort on being user-friendly!)
                Just card -> do
                    insert (LinkPairs cardId (entityKey card))
                    insert (LinkPairs (entityKey card) cardId)
                    return ()
        forM_ types $ \typeName -> do
            -- check if the type already exists in the Type table.
            -- if not, insert it. In either case, insert the card/type many-to-many info
            maybeType <- selectFirst [TypeName ==. typeName] []
            case maybeType of
                Just typeId -> insert $ TypeCard cardId $ entityKey typeId
                Nothing -> do
                    typeId <- insert (Type typeName)
                    insert $ TypeCard cardId typeId


updateCard :: RunDB () -> Text -> CardWithTypesAndLinks -> Handler ()
updateCard runDB name (CardWithTypesAndLinks baseCard types links) = liftIO . runDB $ do
    existingCard <- selectFirst [CardName ==. name] []
    case existingCard of
        Just oldCard -> do
            Database.Persist.Postgresql.replace (entityKey oldCard) baseCard
            -- for simplicity, although it's obviously less efficient, let's
            -- just delete all previous types for the card, and re-add the new ones
            typeCards <- selectList [TypeCardCardId ==. entityKey oldCard] []
            forM_ (entityKey <$> typeCards) delete
            forM_ types $ \typeName -> do
                -- do same as when inserting, check if the type already exists
                maybeType <- selectFirst [TypeName ==. typeName] []
                case maybeType of
                    Just typeId -> insert $ TypeCard (entityKey oldCard) (entityKey typeId)
                    Nothing -> do
                        typeId <- insert (Type typeName)
                        insert $ TypeCard (entityKey oldCard) typeId
            -- same with the links. Need to delete the linked card records and insert whatever is provided
            currentRecords <- selectList ([LinkPairsCardOne ==. entityKey oldCard]
                        ||. [LinkPairsCardTwo ==. entityKey oldCard]) []
            forM_ (map entityKey currentRecords) delete
            linkedCards <- forM links $ \linkName -> selectFirst [CardName ==. linkName] []
            let linkKeys = entityKey <$> catMaybes linkedCards
            forM_ linkKeys $ \linked -> do
                insert $ LinkPairs (entityKey oldCard) linked
                insert $ LinkPairs linked (entityKey oldCard)
        Nothing -> return ()


deleteCard :: RunDB () -> Text -> Handler ()
deleteCard runDB name = liftIO . runDB $ do
    existingCard <- selectFirst [CardName ==. name] []
    case existingCard of
        Just card -> do
            -- delete everything in the join tables
            typeIds <- selectList [TypeCardCardId ==. entityKey card] []
            forM_ (entityKey <$> typeIds) delete
            linkIds <- selectList ([LinkPairsCardOne ==. entityKey card]
                ||. [LinkPairsCardTwo ==. entityKey card])[]
            forM_ (entityKey <$> linkIds) delete
            -- finally remove the deleted card itself
            delete (entityKey card)
        Nothing -> return ()


doWithNoContent :: Handler () -> Handler NoContent
doWithNoContent act = act >> return NoContent


server :: (forall a. RunDB a) -> Server PrivateAPI
server runDB = \_ -> doWithNoContent . insertCard runDB
                :<|> (doWithNoContent .) . updateCard runDB
                :<|> doWithNoContent . deleteCard runDB
