{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text hiding (map, null, foldr)
import Database.Esqueleto
import Servant

import Database
import Instances
import SubsidiaryTypes


type PublicAPI = "cards" :> Get '[JSON] [CardWithTypesAndLinks]
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


handlerWithError :: Handler (Maybe CardWithTypesAndLinks) -> Handler CardWithTypesAndLinks
handlerWithError hdlr = do
    maybeCard <- hdlr
    case maybeCard of
        Just card -> return card
        Nothing -> throwError $ err404 { errBody = "couldn't find a card of that name" }


server :: Server PublicAPI
server = getAllCards
            :<|> getFilteredCards
            :<|> handlerWithError . getOneCard
            :<|> getSets
            :<|> getTypes


publicAPI :: Proxy PublicAPI
publicAPI = Proxy
