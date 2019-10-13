{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text hiding (map, concat, null)
import Database.Persist
import Database.Persist.Sqlite
import GHC.Generics
import Network.Wai
import Servant
import Web.HttpApiData (readTextData)

import Database (Card(..), Type(..), TypeCard(..), EntityField(..), runDBActions, migrateAll)
import SubsidiaryTypes


dbConn :: Text
dbConn = "c:\\Users\\robin\\Documents\\code\\dominion-api\\dbtest.db"


data CardWithTypes = CardWithTypes Card [CardType] deriving (Generic)

instance FromJSON CardWithTypes where
    parseJSON (Object v) =
        CardWithTypes <$> parsedCard <*> v .: "types"
        where parsedCard = parseJSON (Object v)  


instance ToJSON CardWithTypes where
    toJSON (CardWithTypes card types) = Object $ HM.insert "types" (toJSON types) (fromObject $ toJSON card)
        where fromObject (Object obj) = obj
              fromObject _ = HM.empty -- can't happen, but a "sensible" default in case of madness!


type DominionAPI = "cards" :> Get '[JSON] [CardWithTypes]
                    :<|> "cards" :> "filter" :> QueryParams "set" Set
                        :> QueryParam "min-coin-cost" Int
                        :> QueryParam "max-coin-cost" Int :> QueryParam "has-potion" Bool
                        :> QueryParam "has-debt" Bool :> QueryFlag "is-kingdom"
                        :> QueryParam "nonterminal" CanDoItQueryChoice
                        :> QueryParam "village" CanDoItQueryChoice
                        :> QueryParam "no-reduce-hand-size" CanDoItQueryChoice
                        :> QueryParam "draws" CanDoItQueryChoice :> QueryFlag "trasher"
                        :> QueryParams "type" CardType :> Get '[JSON] [CardWithTypes]
                    :<|> "cards" :> Capture "card-name" Text :> Get '[JSON] CardWithTypes
                    :<|> "cards" :> "new" :> ReqBody '[JSON] CardWithTypes :> PostNoContent '[JSON] NoContent
                    :<|> "cards" :> "update" :> Capture "card-name" Text
                        :> ReqBody '[JSON] CardWithTypes :> PutNoContent '[JSON] NoContent
                    :<|> "cards" :> "delete" :> Capture "card-name" Text :> PostNoContent '[JSON] NoContent


data CanDoItQueryChoice = CanSometimes | CanAlways deriving (Read)

instance FromHttpApiData CanDoItQueryChoice where
    parseQueryParam = readTextData


possibleChoices :: CanDoItQueryChoice -> [CanDoIt]
possibleChoices CanSometimes = [Always, Sometimes]
possibleChoices CanAlways = [Always]


getTypes :: Entity Card -> IO (Maybe CardWithTypes)
getTypes card = runDBActions dbConn $ do
    typeCards <- selectList [TypeCardCardId ==. entityKey card] []
    types <- selectList [TypeId <-. map ((\(TypeCard _ tid) -> tid) . entityVal) typeCards] []
    return . Just . CardWithTypes (entityVal card)
        . map ((\(Type name) -> name) . entityVal) $ types


getAllCards :: Handler [CardWithTypes]
getAllCards = liftIO . runDBActions dbConn $ do
    allCards <- selectList [] []
    liftIO . fmap catMaybes . sequence $ getTypes <$> allCards


getOneCard :: Text -> Handler (Maybe CardWithTypes)
getOneCard name = liftIO . runDBActions dbConn $ do
    maybeCard <- selectFirst [CardName ==. name] []
    case maybeCard of
        Nothing -> return Nothing
        Just card -> liftIO . getTypes $ card


getFilteredCards :: [Set] -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool
                        -> Bool -> Maybe CanDoItQueryChoice
                        -> Maybe CanDoItQueryChoice -> Maybe CanDoItQueryChoice
                        -> Maybe CanDoItQueryChoice -> Bool -> [CardType]
                        -> Handler [CardWithTypes]
getFilteredCards sets maybeMinCost maybeMaxCost maybeNeedsPotion maybeNeedsDebt
        mustBeKingdom maybeNonTerminal maybeVillage maybeNoHandsizeReduction
        maybeDraws mustTrash types
        = liftIO . runDBActions dbConn $ do
            filteredExceptTypes <- selectList queries []
            correctTypes <- selectList typeQuery []
            joinTableItems <- selectList [TypeCardTypeId <-. map entityKey correctTypes] []
            filteredCards <- selectList [CardId <-. map entityKey filteredExceptTypes,
                                CardId <-. map ((\(TypeCard cid _) -> cid). entityVal) joinTableItems] []
            liftIO . fmap catMaybes . sequence $ getTypes <$> filteredCards
            where queries = setsQuery ++ minCostQuery ++ maxCostQuery ++ potionQuery
                                ++ debtQuery ++ kingdomQuery ++ nonTerminalQuery
                                ++ villageQuery ++ noHandSizeReductionQuery
                                ++ drawsQuery ++ trashQuery
                  setsQuery = if null sets then [] else [CardSet <-. sets]
                  minCostQuery = case maybeMinCost of
                                    Nothing -> []
                                    Just minCost -> [CardCoinCost !=. Nothing,
                                                        CardCoinCost >=. Just minCost]
                  maxCostQuery = case maybeMaxCost of
                                    Nothing -> []
                                    Just maxCost -> [CardCoinCost !=. Nothing,
                                                        CardCoinCost <=. Just maxCost]
                  potionQuery = case maybeNeedsPotion of
                                    Nothing -> []
                                    Just needsPotion -> [CardPotionCost ==. Just needsPotion]
                  debtQuery = case maybeNeedsDebt of
                                Nothing -> []
                                Just True -> [CardDebtCost !=. Nothing,
                                                CardDebtCost >. Just 0]
                                Just False -> [CardDebtCost ==. Just 0]
                  kingdomQuery = if mustBeKingdom then [CardIsKingdom ==. True] else []
                  nonTerminalQuery = case maybeNonTerminal of
                                        Nothing -> []
                                        Just choice -> [CardNonTerminal <-. map Just (possibleChoices choice)]
                  villageQuery = case maybeVillage of
                                    Nothing -> []
                                    Just choice -> [CardGivesExtraActions <-.
                                                        map Just (possibleChoices choice)]
                  noHandSizeReductionQuery = case maybeNoHandsizeReduction of
                                                Nothing -> []
                                                Just choice -> [CardReturnsCard <-.
                                                                    map Just (possibleChoices choice)]
                  drawsQuery = case maybeDraws of
                                Nothing -> []
                                Just choice -> [CardIncreasesHandSize <-.
                                                    map Just (possibleChoices choice)]
                  trashQuery = if mustTrash then [CardTrashes ==. True] else []
                  typeQuery = if null types then [] else [TypeName <-. types]


insertCard :: CardWithTypes -> Handler ()
insertCard (CardWithTypes baseCard types) =
    liftIO . runDBActions dbConn $ do
        runMigration migrateAll
        cardId <- insert baseCard
        forM_ types $ \typeName -> do
            -- check if the type already exists in the Type table.
            -- if not, insert it. In either case, insert the card/type many-to-many info
            maybeType <- selectFirst [TypeName ==. typeName] []
            case maybeType of
                Just typeId -> insert $ TypeCard cardId $ entityKey typeId
                Nothing -> do
                    typeId <- insert (Type typeName)
                    insert $ TypeCard cardId typeId


updateCard :: Text -> CardWithTypes -> Handler ()
-- similar adjustments needed here
updateCard name (CardWithTypes baseCard types) = liftIO . runDBActions dbConn $ do
    existingCard <- selectFirst [CardName ==. name] []
    case existingCard of
        Just oldCard -> do
            Database.Persist.Sqlite.replace (entityKey oldCard) baseCard
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
        Nothing -> return ()


deleteCard :: Text -> Handler ()
deleteCard name = liftIO . runDBActions dbConn $ do
    existingCard <- selectFirst [CardName ==. name] []
    case existingCard of
        Just card -> do
            -- delete everything in the join table
            typeIds <- selectList [TypeCardCardId ==. entityKey card] []
            forM_ (entityKey <$> typeIds) delete
            delete (entityKey card)
        Nothing -> return ()


doWithNoContent :: Handler () -> Handler NoContent
doWithNoContent act = act >> return NoContent


handlerWithError :: Handler (Maybe CardWithTypes) -> Handler CardWithTypes
handlerWithError hdlr = do
    maybeCard <- hdlr
    case maybeCard of
        Just card -> return card
        Nothing -> throwError $ err404 { errBody = "couldn't find a card of that name" }


server :: Server DominionAPI
server = getAllCards
            :<|> getFilteredCards
            :<|> handlerWithError . getOneCard
            :<|> doWithNoContent . insertCard
            :<|> (doWithNoContent .) . updateCard
            :<|> doWithNoContent . deleteCard


dominionAPI :: Proxy DominionAPI
dominionAPI = Proxy

api :: Application
api = serve dominionAPI server
