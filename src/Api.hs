{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Text hiding (map, concat, null)
import Database.Persist
import Database.Persist.Sql (runMigration)
import Database.Persist.Postgresql (replace)
import Network.Wai
import Servant
import Web.HttpApiData (readTextData)

import Database (Card(..), Type(..), TypeCard(..), CardLinks(..), EntityField(..), runDBActions, migrateAll)
import Instances
import SubsidiaryTypes



type DominionAPI = "cards" :> Get '[JSON] [CardWithTypesAndLinks]
                    :<|> "cards" :> "filter" :> QueryParams "set" Set
                        :> QueryParam "min-coin-cost" Int
                        :> QueryParam "max-coin-cost" Int :> QueryParam "has-potion" Bool
                        :> QueryParam "has-debt" Bool :> QueryFlag "is-kingdom"
                        :> QueryParam "nonterminal" CanDoItQueryChoice
                        :> QueryParam "village" CanDoItQueryChoice
                        :> QueryParam "no-reduce-hand-size" CanDoItQueryChoice
                        :> QueryParam "draws" CanDoItQueryChoice :> QueryFlag "trasher"
                        :> QueryParams "type" CardType :> QueryParams "linked" Text
                        :> Get '[JSON] [CardWithTypesAndLinks]
                    :<|> "cards" :> Capture "card-name" Text :> Get '[JSON] CardWithTypesAndLinks
                    :<|> "cards" :> "new" :> ReqBody '[JSON] CardWithTypesAndLinks :> PostNoContent '[JSON] NoContent
                    :<|> "cards" :> "update" :> Capture "card-name" Text
                        :> ReqBody '[JSON] CardWithTypesAndLinks :> PutNoContent '[JSON] NoContent
                    :<|> "cards" :> "delete" :> Capture "card-name" Text :> DeleteNoContent '[JSON] NoContent


getTypesAndLinks :: Entity Card -> IO (Maybe CardWithTypesAndLinks)
getTypesAndLinks card = runDBActions $ do
    typeCards <- selectList [TypeCardCardId ==. entityKey card] []
    types <- selectList [TypeId <-. map (typeCardTypeId . entityVal) typeCards] []
    maybeLinks <- selectFirst [CardLinksCardId ==. entityKey card] []
    case maybeLinks of
        Nothing -> return Nothing
        Just links -> do
            actualCards <- selectList [CardId <-. (cardLinksLinkedCards . entityVal) links] []
            return . Just $ CardWithTypesAndLinks (entityVal card)
                (map (typeName . entityVal) types)
                (map (cardName . entityVal) actualCards)


getAllCards :: Handler [CardWithTypesAndLinks]
getAllCards = liftIO . runDBActions $ do
    allCards <- selectList [] []
    liftIO . fmap catMaybes . sequence $ getTypesAndLinks <$> allCards


getOneCard :: Text -> Handler (Maybe CardWithTypesAndLinks)
getOneCard name = liftIO . runDBActions $ do
    maybeCard <- selectFirst [CardName ==. name] []
    case maybeCard of
        Nothing -> return Nothing
        Just card -> liftIO . getTypesAndLinks $ card


getFilteredCards :: [Set] -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool
                        -> Bool -> Maybe CanDoItQueryChoice
                        -> Maybe CanDoItQueryChoice -> Maybe CanDoItQueryChoice
                        -> Maybe CanDoItQueryChoice -> Bool -> [CardType] -> [Text]
                        -> Handler [CardWithTypesAndLinks]
getFilteredCards sets maybeMinCost maybeMaxCost maybeNeedsPotion maybeNeedsDebt
        mustBeKingdom maybeNonTerminal maybeVillage maybeNoHandsizeReduction
        maybeDraws mustTrash types links
        = liftIO . runDBActions $ do
            filteredExceptTypesAndLinks <- selectList queries []
            correctTypes <- selectList typeQuery []
            joinTableItems <- selectList [TypeCardTypeId <-. map entityKey correctTypes] []
            filteredExceptLinks <- selectList [CardId <-. map entityKey filteredExceptTypesAndLinks,
                                      CardId <-. map (typeCardCardId . entityVal) joinTableItems] []
            filteredCards <- if null links
                then return filteredExceptLinks
                else do
                    cardsToLinkTo <- selectList [CardName <-. links] []
                    linkedCards <- forM cardsToLinkTo $ \cardToLink -> do
                        maybeLinked <- selectFirst [CardLinksCardId ==. entityKey cardToLink] []
                        case maybeLinked of
                            Nothing -> return []
                            Just linked -> return . cardLinksLinkedCards . entityVal $ linked
                    selectList [CardId <-. map entityKey filteredExceptLinks,
                        CardId <-. concat linkedCards] []
            liftIO . fmap catMaybes . sequence $ getTypesAndLinks <$> filteredCards
            where queries = setsQuery ++ minCostQuery ++ maxCostQuery ++ potionQuery
                                ++ debtQuery ++ kingdomQuery ++ nonTerminalQuery
                                ++ villageQuery ++ noHandSizeReductionQuery
                                ++ drawsQuery ++ trashQuery
                  setsQuery
                    | null sets = []
                    | otherwise = let filloutBase = if BaseFirstEd `elem` sets || BaseSecondEd `elem` sets
                                                    then Base:sets
                                                    else sets
                                      allSets = if IntrigueFirstEd `elem` sets || IntrigueSecondEd `elem` sets
                                                then Intrigue:filloutBase
                                                else filloutBase
                                  in [CardSet <-. allSets]
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


insertCard :: CardWithTypesAndLinks -> Handler ()
insertCard (CardWithTypesAndLinks baseCard types links) =
    liftIO . runDBActions $ do
        runMigration migrateAll
        cardId <- insert baseCard
        -- insert linked cards, "both ways". First insert them directly for the card under consideration
        maybeLinkedIds <- forM links $ \cardName -> do
            maybeCard <- selectFirst [CardName ==. cardName] []
            case maybeCard of
                Nothing -> return Nothing -- if no card exists with the give name, we'll just ignore it
                -- (would be better to throw an error, but this is simpler, and will only affect me
                -- so I don't have to expend too much effort on being user-friendly!)
                Just card -> do
                    -- also make sure, for each of the linked cards that exist,
                    -- that the given card is added to its set of links
                    maybeCardWithLinks <- selectFirst [CardLinksCardId ==. entityKey card] []
                    case maybeCardWithLinks of
                        Nothing -> return ()
                        Just cardWithLinks ->
                            let alreadyLinked = cardLinksLinkedCards (entityVal cardWithLinks) in
                            if cardId `elem` alreadyLinked
                                then return () -- already there, nothing to do
                                else do
                                    delete (entityKey cardWithLinks)
                                    insert $ CardLinks (cardLinksCardId . entityVal $ cardWithLinks)
                                        (cardId : alreadyLinked)
                                    return ()
                    return . Just . entityKey $ card
        insert $ CardLinks cardId $ catMaybes maybeLinkedIds
        forM_ types $ \typeName -> do
            -- check if the type already exists in the Type table.
            -- if not, insert it. In either case, insert the card/type many-to-many info
            maybeType <- selectFirst [TypeName ==. typeName] []
            case maybeType of
                Just typeId -> insert $ TypeCard cardId $ entityKey typeId
                Nothing -> do
                    typeId <- insert (Type typeName)
                    insert $ TypeCard cardId typeId


updateCard :: Text -> CardWithTypesAndLinks -> Handler ()
-- similar adjustments needed here
updateCard name (CardWithTypesAndLinks baseCard types links) = liftIO . runDBActions $ do
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
            -- same with the links. Need to delete the linked card record and insert whatever is provided
            linkedCards <- forM links $ \linkName -> selectFirst [CardName ==. linkName] []
            maybeLinkRecord <- selectFirst [CardLinksCardId ==. entityKey oldCard] []
            case maybeLinkRecord of
                Nothing -> return ()
                Just linkRecord -> do
                    delete $ entityKey linkRecord
                    insert $ CardLinks (cardLinksCardId . entityVal $ linkRecord)
                        (entityKey <$> catMaybes linkedCards)
                    return ()
            -- note, if the update removes cards from the linked list, this doesn't reciprocally remove the
            -- updated card from those cards linked-lists. Doesn't seem critical though, might add to the
            -- todo list.
        Nothing -> return ()


deleteCard :: Text -> Handler ()
deleteCard name = liftIO . runDBActions $ do
    existingCard <- selectFirst [CardName ==. name] []
    case existingCard of
        Just card -> do
            -- delete everything in the join tables
            typeIds <- selectList [TypeCardCardId ==. entityKey card] []
            forM_ (entityKey <$> typeIds) delete
            linkIds <- selectList [CardLinksCardId ==. entityKey card] []
            forM_ (entityKey <$> linkIds) delete
            -- remove the linked card from ALL link records
            allLinks <- selectList [] []
            forM_ allLinks $ \linkRecord -> do
                when (entityKey card `elem` cardLinksLinkedCards (entityVal linkRecord)) $ do
                    delete (entityKey linkRecord)
                    insert $ CardLinks (cardLinksCardId $ entityVal linkRecord)
                        $ L.delete (entityKey card) $ cardLinksLinkedCards (entityVal linkRecord)
                    return ()
            delete (entityKey card)
        Nothing -> return ()


doWithNoContent :: Handler () -> Handler NoContent
doWithNoContent act = act >> return NoContent


handlerWithError :: Handler (Maybe CardWithTypesAndLinks) -> Handler CardWithTypesAndLinks
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
