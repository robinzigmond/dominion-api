{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Text hiding (map, concat, null, foldr, head, filter)
import Database.Esqueleto
import qualified Database.Persist as P
import Database.Persist.Sql (runMigration)
import Database.Persist.Postgresql (replace)
import Network.Wai
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


type DominionAPI = "cards" :> Get '[JSON] [CardWithTypesAndLinks]
                    :<|> "cards" :> "filter" :> QueryParams "set" Set
                        :> QueryParam "min-coin-cost" Int
                        :> QueryParam "max-coin-cost" Int :> QueryParam "has-potion" Bool
                        :> QueryParam "has-debt" Bool :> QueryFlag "is-kingdom"
                        :> QueryParam "nonterminal" CanDoItQueryChoice
                        :> QueryParam "village" CanDoItQueryChoice
                        :> QueryParam "no-reduce-hand-size" CanDoItQueryChoice
                        :> QueryParam "draws" CanDoItQueryChoice :> QueryFlag "trasher"
                        :> QueryParam "extra-buy" CanDoItQueryChoice
                        :> QueryParams "type" CardType :> QueryParams "linked" Text
                        :> Get '[JSON] [CardWithTypesAndLinks]
                    :<|> "cards" :> Capture "card-name" Text :> Get '[JSON] CardWithTypesAndLinks
                    :<|> "sets" :> Get '[JSON] [Set]
                    :<|> "types" :> Get '[JSON] [CardType]
                    :<|> BasicAuth "dominion" () :> (
                        "cards" :> "new" :> ReqBody '[JSON] CardWithTypesAndLinks
                            :> PostNoContent '[JSON] NoContent
                        :<|> "cards" :> "update" :> Capture "card-name" Text
                            :> ReqBody '[JSON] CardWithTypesAndLinks :> PutNoContent '[JSON] NoContent
                        :<|> "cards" :> "delete" :> Capture "card-name" Text
                            :> DeleteNoContent '[JSON] NoContent
                    )


getAllCards :: Handler [CardWithTypesAndLinks]
getAllCards = liftIO . runDBActions $ do
    sqlres <- select $
        from $ \(c `InnerJoin` tc `InnerJoin` t `LeftOuterJoin` lp `LeftOuterJoin` c1) -> do
            on (c1 ?. CardId ==. lp ?. LinkPairsCardTwo)
            on (just (c ^. CardId) ==. lp ?. LinkPairsCardOne)
            on (t ^. TypeId ==. tc ^. TypeCardTypeId)
            on (c ^. CardId ==. tc ^. TypeCardCardId)
            return (c, t, c1)
    return $ foldr merge [] sqlres
        where
            resolveCard c t (Just c1) = CardWithTypesAndLinks (entityVal c) [typeName . entityVal $ t]
                [cardName . entityVal $ c1]
            resolveCard c t Nothing = CardWithTypesAndLinks (entityVal c) [typeName . entityVal $ t] []
            mergeCards (c, t, Just c1) (CardWithTypesAndLinks card types links)
                = CardWithTypesAndLinks card (typeName (entityVal  t) : types)
                    (cardName (entityVal c1) : links)
            mergeCards (c, t, Nothing) (CardWithTypesAndLinks card types links)
                = CardWithTypesAndLinks card (typeName (entityVal  t) : types) links
            merge (c, t, c1) [] = [resolveCard c t c1]
            merge (c, t, c1) (cd@(CardWithTypesAndLinks card _ _) : cs)
                | cardName (entityVal c) == cardName card
                    = mergeCards (c, t, c1) cd : cs
                | otherwise = cd : merge (c, t, c1) cs


getOneCard :: Text -> Handler (Maybe CardWithTypesAndLinks)
getOneCard name = liftIO . runDBActions $ do
    sqlres <- select $
        from $ \(c `InnerJoin` tc `InnerJoin` t `LeftOuterJoin` lp `LeftOuterJoin` c1) -> do
            where_  (c ^. CardName ==. val name)
            on (c1 ?. CardId ==. lp ?. LinkPairsCardTwo)
            on (lp ?. LinkPairsCardOne ==. just (c ^. CardId))
            on (t ^. TypeId ==. tc ^. TypeCardTypeId)
            on (c ^. CardId ==. tc ^. TypeCardCardId)
            return (c, t, c1)
    return $ foldr merge Nothing sqlres
        where merge (c, t, Just c1) Nothing
                = Just $ CardWithTypesAndLinks (entityVal c) [typeName . entityVal $ t]
                    [cardName . entityVal $ c1]
              merge (c, t, Nothing) Nothing
                = Just $ CardWithTypesAndLinks (entityVal c) [typeName . entityVal $ t] []
              merge (c, t, Just c1) (Just (CardWithTypesAndLinks card types links))
                = Just $ CardWithTypesAndLinks card (typeName (entityVal  t) : types)
                    (cardName (entityVal c1) : links)
              merge (c, t, Nothing) (Just (CardWithTypesAndLinks card types links))
                = Just $ CardWithTypesAndLinks card (typeName (entityVal  t) : types) links


getFilteredCards :: [Set] -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool
                        -> Bool -> Maybe CanDoItQueryChoice
                        -> Maybe CanDoItQueryChoice -> Maybe CanDoItQueryChoice
                        -> Maybe CanDoItQueryChoice -> Bool -> Maybe CanDoItQueryChoice
                        -> [CardType] -> [Text]
                        -> Handler [CardWithTypesAndLinks]
getFilteredCards sets maybeMinCost maybeMaxCost maybeNeedsPotion maybeNeedsDebt
        mustBeKingdom maybeNonTerminal maybeVillage maybeNoHandsizeReduction
        maybeDraws mustTrash maybeExtraBuy types links
        = liftIO . runDBActions $ do
            sqlres <- select $
                from $ \(c `InnerJoin` tc `InnerJoin` t `LeftOuterJoin` lp `LeftOuterJoin` c1) -> do
                    let filloutBase = if BaseFirstEd `elem` sets || BaseSecondEd `elem` sets
                        then Base:sets
                        else sets
                    let allSets = if IntrigueFirstEd `elem` sets || IntrigueSecondEd `elem` sets
                        then Intrigue:filloutBase
                        else filloutBase
                    let setsQuery = if null sets
                                        then []
                                        else [c ^. CardSet `in_` valList allSets]
                    let minCostQuery = case maybeMinCost of
                            Nothing -> []
                            Just minCost -> [not_ . isNothing $ c ^. CardCoinCost,
                                                c ^. CardCoinCost >=. val (Just minCost)]
                    let maxCostQuery = case maybeMaxCost of
                            Nothing -> []
                            Just maxCost -> [not_ . isNothing $ c ^. CardCoinCost,
                                                c ^. CardCoinCost <=. val (Just maxCost)]
                    let potionQuery = case maybeNeedsPotion of
                            Nothing -> []
                            Just needsPotion -> [c ^. CardPotionCost ==. val (Just needsPotion)]
                    let debtQuery = case maybeNeedsDebt of
                            Nothing -> []
                            Just True -> [not_ . isNothing $ c ^. CardDebtCost,
                                            c ^. CardDebtCost >. val (Just 0)]
                            Just False -> [c ^. CardDebtCost ==. val (Just 0)]
                    let kingdomQuery = if mustBeKingdom then [c ^. CardIsKingdom ==.val  True] else []
                    let nonTerminalQuery = case maybeNonTerminal of
                            Nothing -> []
                            Just choice ->
                                [c ^. CardNonTerminal `in_` valList (map Just (possibleChoices choice))]
                    let villageQuery = case maybeVillage of
                            Nothing -> []
                            Just choice ->
                                [c ^. CardGivesExtraActions `in_` valList (map Just (possibleChoices choice))]
                    let noHandSizeReductionQuery = case maybeNoHandsizeReduction of
                            Nothing -> []
                            Just choice ->
                                [c ^. CardReturnsCard `in_` valList (map Just (possibleChoices choice))]
                    let drawsQuery = case maybeDraws of
                            Nothing -> []
                            Just choice ->
                                [c ^. CardIncreasesHandSize `in_` valList (map Just (possibleChoices choice))]
                    let trashQuery = if mustTrash then [c ^. CardTrashes ==. val True] else []
                    let extraBuyQuery = case maybeExtraBuy of
                            Nothing -> []
                            Just choice ->
                                [c ^. CardExtraBuy `in_` valList (map Just (possibleChoices choice))]
                    let typesQuery = if null types
                                        then []
                                        else [t ^. TypeName `in_` valList types]
                    let linksQuery = if null links
                                        then []
                                        else [c1 ?. CardName `in_` valList (map Just links)]
                    let queries = setsQuery ++ minCostQuery ++ maxCostQuery ++ potionQuery
                            ++ debtQuery ++ kingdomQuery ++ nonTerminalQuery
                            ++ villageQuery ++ noHandSizeReductionQuery ++ drawsQuery
                            ++ trashQuery ++ extraBuyQuery ++ typesQuery ++ linksQuery
                    forM_ queries where_
                    on (c1 ?. CardId ==. lp ?. LinkPairsCardTwo)
                    on (lp ?. LinkPairsCardOne ==. just (c ^. CardId))
                    on (t ^. TypeId ==. tc ^. TypeCardTypeId)
                    on (c ^. CardId ==. tc ^. TypeCardCardId)
                    return (c, t, c1)
            return $ foldr merge [] sqlres
                where
                    resolveCard c t (Just c1) = CardWithTypesAndLinks (entityVal c) [typeName . entityVal $ t]
                        [cardName . entityVal $ c1]
                    resolveCard c t Nothing = CardWithTypesAndLinks (entityVal c) [typeName . entityVal $ t] []    
                    mergeCards (c, t, Just c1) (CardWithTypesAndLinks card types links)
                        = CardWithTypesAndLinks card (typeName (entityVal  t) : types)
                            (cardName (entityVal c1) : links)
                    mergeCards (c, t, Nothing) (CardWithTypesAndLinks card types links)
                        = CardWithTypesAndLinks card (typeName (entityVal  t) : types) links
                    merge (c, t, c1) [] = [resolveCard c t c1]
                    merge (c, t, c1) (cd@(CardWithTypesAndLinks card _ _) : cs)
                        | cardName (entityVal c) == cardName card
                            = mergeCards (c, t, c1) cd : cs
                        | otherwise = cd : merge (c, t, c1) cs


getSets :: Handler [Set]
getSets = return [minBound..maxBound]


getTypes :: Handler [CardType]
getTypes = return [minBound..maxBound]


insertCard :: CardWithTypesAndLinks -> Handler ()
insertCard (CardWithTypesAndLinks baseCard types links) =
    liftIO . runDBActions $ do
        runMigration migrateAll
        cardId <- P.insert baseCard
        -- insert linked cards
        forM links $ \cardName -> do
            maybeCard <- P.selectFirst [CardName P.==. cardName] []
            case maybeCard of
                Nothing -> return () -- if no card exists with the give name, we'll just ignore it
                -- (would be better to throw an error, but this is simpler, and will only affect me
                -- so I don't have to expend too much effort on being user-friendly!)
                Just card -> do
                    P.insert (LinkPairs cardId (entityKey card))
                    P.insert (LinkPairs (entityKey card) cardId)
                    return ()
        forM_ types $ \typeName -> do
            -- check if the type already exists in the Type table.
            -- if not, insert it. In either case, insert the card/type many-to-many info
            maybeType <- P.selectFirst [TypeName P.==. typeName] []
            case maybeType of
                Just typeId -> P.insert $ TypeCard cardId $ entityKey typeId
                Nothing -> do
                    typeId <- P.insert (Type typeName)
                    P.insert $ TypeCard cardId typeId


updateCard :: Text -> CardWithTypesAndLinks -> Handler ()
updateCard name (CardWithTypesAndLinks baseCard types links) = liftIO . runDBActions $ do
    existingCard <- P.selectFirst [CardName P.==. name] []
    case existingCard of
        Just oldCard -> do
            Database.Persist.Postgresql.replace (entityKey oldCard) baseCard
            -- for simplicity, although it's obviously less efficient, let's
            -- just delete all previous types for the card, and re-add the new ones
            typeCards <- P.selectList [TypeCardCardId P.==. entityKey oldCard] []
            forM_ (entityKey <$> typeCards) P.delete
            forM_ types $ \typeName -> do
                -- do same as when inserting, check if the type already exists
                maybeType <- P.selectFirst [TypeName P.==. typeName] []
                case maybeType of
                    Just typeId -> insert $ TypeCard (entityKey oldCard) (entityKey typeId)
                    Nothing -> do
                        typeId <- insert (Type typeName)
                        insert $ TypeCard (entityKey oldCard) typeId
            -- same with the links. Need to delete the linked card records and insert whatever is provided
            currentRecords <- P.selectList ([LinkPairsCardOne P.==. entityKey oldCard]
                        P.||. [LinkPairsCardTwo P.==. entityKey oldCard]) []
            forM_ (map entityKey currentRecords) P.delete
            linkedCards <- forM links $ \linkName -> P.selectFirst [CardName P.==. linkName] []
            let linkKeys = entityKey <$> catMaybes linkedCards
            forM_ linkKeys $ \linked -> do
                insert $ LinkPairs (entityKey oldCard) linked
                insert $ LinkPairs linked (entityKey oldCard)
        Nothing -> return ()


deleteCard :: Text -> Handler ()
deleteCard name = liftIO . runDBActions $ do
    existingCard <- P.selectFirst [CardName P.==. name] []
    case existingCard of
        Just card -> do
            -- delete everything in the join tables
            typeIds <- P.selectList [TypeCardCardId P.==. entityKey card] []
            forM_ (entityKey <$> typeIds) P.delete
            linkIds <- P.selectList ([LinkPairsCardOne P.==. entityKey card]
                P.||. [LinkPairsCardTwo P.==. entityKey card])[]
            forM_ (entityKey <$> linkIds) P.delete
            -- finally remove the deleted card itself
            P.delete (entityKey card)
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
            :<|> getSets
            :<|> getTypes
            :<|> (\_ -> doWithNoContent . insertCard
                :<|> (doWithNoContent .) . updateCard
                :<|> doWithNoContent . deleteCard
            )


dominionAPI :: Proxy DominionAPI
dominionAPI = Proxy


api :: Application
api = serveWithContext dominionAPI authContext server
