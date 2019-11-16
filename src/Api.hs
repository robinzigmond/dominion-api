{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.Error.Class (catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B (pack)
import qualified Data.Set as S (toList, fromList)
import Data.Text hiding (map, null, foldr)
import Database.Esqueleto
import Servant

import Database
import Instances
import SubsidiaryTypes


{- taken from Alp Mestanogullari's Stack Overflow answer here:
   https://stackoverflow.com/questions/51146315/servant-queryparams-parse-error
   Not sure why Servant makes it impossible to recover from query param parsing failures,
   or why the documentation seems to contain no word of this relatively simple way to fix it -}
type RecoverableQueryParam = QueryParam' '[Optional, Lenient]


type PublicAPI = "cards" :> Get '[JSON] (WithError [CardWithTypesAndLinks])
                    :<|> "cards" :> "filter" :> QueryParams "set" Set
                        :> RecoverableQueryParam "min-coin-cost" Int
                        :> RecoverableQueryParam "max-coin-cost" Int :> RecoverableQueryParam "has-potion" Bool
                        :> RecoverableQueryParam "has-debt" Bool :> QueryFlag "is-kingdom"
                        :> RecoverableQueryParam "nonterminal" CanDoItQueryChoice
                        :> RecoverableQueryParam "village" CanDoItQueryChoice
                        :> RecoverableQueryParam "no-reduce-hand-size" CanDoItQueryChoice
                        :> RecoverableQueryParam "draws" CanDoItQueryChoice :> QueryFlag "trasher"
                        :> RecoverableQueryParam "extra-buy" CanDoItQueryChoice
                        :> QueryParams "type" CardType :> QueryParams "linked" Text
                        :> Get '[JSON] (WithError [CardWithTypesAndLinks])
                    :<|> "cards" :> Capture "card-name" Text
                        :> Get '[JSON] (WithError CardWithTypesAndLinks)
                    :<|> "sets" :> Get '[JSON] (WithError [Set])
                    :<|> "types" :> Get '[JSON] (WithError [CardType])


getAllCards :: Handler (WithError [CardWithTypesAndLinks])
getAllCards = showError . liftIO . runDBActions $ do
    sqlres <- select $
        from $ \(c `InnerJoin` tc `InnerJoin` t `LeftOuterJoin` lp `LeftOuterJoin` c1) -> do
            on (c1 ?. CardId ==. lp ?. LinkPairsCardTwo)
            on (just (c ^. CardId) ==. lp ?. LinkPairsCardOne)
            on (t ^. TypeId ==. tc ^. TypeCardTypeId)
            on (c ^. CardId ==. tc ^. TypeCardCardId)
            return (c, t, c1)
    return . map uniques $ foldr merge [] sqlres
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
            noRepeats :: (Ord a) => [a] -> [a]
            noRepeats = S.toList . S.fromList
            uniques (CardWithTypesAndLinks c ts ls)
                = CardWithTypesAndLinks c (noRepeats ts) (noRepeats ls)


getOneCard :: Text -> Handler (WithError CardWithTypesAndLinks)
getOneCard name = showError . handlerWithError . liftIO . runDBActions $ do
    sqlres <- select $
        from $ \(c `InnerJoin` tc `InnerJoin` t `LeftOuterJoin` lp `LeftOuterJoin` c1) -> do
            where_  (c ^. CardName ==. val name)
            on (c1 ?. CardId ==. lp ?. LinkPairsCardTwo)
            on (lp ?. LinkPairsCardOne ==. just (c ^. CardId))
            on (t ^. TypeId ==. tc ^. TypeCardTypeId)
            on (c ^. CardId ==. tc ^. TypeCardCardId)
            return (c, t, c1)
    return . fmap uniques $ foldr merge Nothing sqlres
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
              noRepeats :: (Ord a) => [a] -> [a]
              noRepeats = S.toList . S.fromList
              uniques (CardWithTypesAndLinks c ts ls)
                = CardWithTypesAndLinks c (noRepeats ts) (noRepeats ls)


getFilteredCards :: [Set] -> Maybe (Either Text Int) -> Maybe (Either Text Int)
                        -> Maybe (Either Text Bool) -> Maybe (Either Text Bool)
                        -> Bool -> Maybe (Either Text CanDoItQueryChoice)
                        -> Maybe (Either Text CanDoItQueryChoice)
                        -> Maybe (Either Text CanDoItQueryChoice)
                        -> Maybe (Either Text CanDoItQueryChoice)
                        -> Bool -> Maybe (Either Text CanDoItQueryChoice)
                        -> [CardType] -> [Text]
                        -> Handler (WithError [CardWithTypesAndLinks])
getFilteredCards sets maybeMinCost maybeMaxCost maybeNeedsPotion maybeNeedsDebt
        mustBeKingdom maybeNonTerminal maybeVillage maybeNoHandsizeReduction
        maybeDraws mustTrash maybeExtraBuy types links
        = showError . checkForError . liftIO . runDBActions $ do
            sqlres <- select $
                from $ \(c `InnerJoin` tc `InnerJoin` t `LeftOuterJoin` lp `LeftOuterJoin` c1
                        `FullOuterJoin` lp1 `FullOuterJoin` c2) -> do
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
                            Just (Right minCost) -> [not_ . isNothing $ c ^. CardCoinCost,
                                                        c ^. CardCoinCost >=. val (Just minCost)]
                    let maxCostQuery = case maybeMaxCost of
                            Nothing -> []
                            Just (Right maxCost) -> [not_ . isNothing $ c ^. CardCoinCost,
                                                        c ^. CardCoinCost <=. val (Just maxCost)]
                    let potionQuery = case maybeNeedsPotion of
                            Nothing -> []
                            Just (Right needsPotion) -> [c ^. CardPotionCost ==. val (Just needsPotion)]
                    let debtQuery = case maybeNeedsDebt of
                            Nothing -> []
                            Just (Right True) -> [not_ . isNothing $ c ^. CardDebtCost,
                                                    c ^. CardDebtCost >. val (Just 0)]
                            Just (Right False) -> [c ^. CardDebtCost ==. val (Just 0)]
                    let kingdomQuery = if mustBeKingdom then [c ^. CardIsKingdom ==.val  True] else []
                    let nonTerminalQuery = case maybeNonTerminal of
                            Nothing -> []
                            Just (Right choice) ->
                                [c ^. CardNonTerminal `in_` valList (map Just (possibleChoices choice))]
                    let villageQuery = case maybeVillage of
                            Nothing -> []
                            Just (Right choice) ->
                                [c ^. CardGivesExtraActions `in_` valList (map Just (possibleChoices choice))]
                    let noHandSizeReductionQuery = case maybeNoHandsizeReduction of
                            Nothing -> []
                            Just (Right choice) ->
                                [c ^. CardReturnsCard `in_` valList (map Just (possibleChoices choice))]
                    let drawsQuery = case maybeDraws of
                            Nothing -> []
                            Just (Right choice) ->
                                [c ^. CardIncreasesHandSize `in_` valList (map Just (possibleChoices choice))]
                    let trashQuery = if mustTrash then [c ^. CardTrashes ==. val True] else []
                    let extraBuyQuery = case maybeExtraBuy of
                            Nothing -> []
                            Just (Right choice) ->
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
                    on (lp1 ?. LinkPairsCardTwo ==. c2 ?. CardId)
                    on (lp1 ?. LinkPairsCardOne ==. lp ?. LinkPairsCardOne)
                    on (c1 ?. CardId ==. lp ?. LinkPairsCardTwo)
                    on (lp ?. LinkPairsCardOne ==. just (c ^. CardId))
                    on (t ^. TypeId ==. tc ^. TypeCardTypeId)
                    on (c ^. CardId ==. tc ^. TypeCardCardId)
                    return (c, t, c2)
            return . map uniques $ foldr merge [] sqlres
                where
                    checkForError act = case checkQueryErrors [maybeMinCost, maybeMaxCost]
                            <|> checkQueryErrors [maybeNeedsPotion, maybeNeedsDebt]
                            <|> checkQueryErrors [maybeNonTerminal, maybeVillage, maybeNoHandsizeReduction,
                                maybeDraws, maybeExtraBuy] of
                        Just e -> throwQueryError . B.pack $ unpack e
                        Nothing -> act

                    checkQueryErrors :: [Maybe (Either e a)] -> Maybe e
                    checkQueryErrors [] = Nothing
                    checkQueryErrors (Just (Left e):xs) = Just e
                    checkQueryErrors (_:xs) = checkQueryErrors xs

                    throwQueryError e = throwError $ err422 { errBody = e }

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

                    noRepeats :: (Ord a) => [a] -> [a]
                    noRepeats = S.toList . S.fromList

                    uniques (CardWithTypesAndLinks c ts ls)
                        = CardWithTypesAndLinks c (noRepeats ts) (noRepeats ls)


getSets :: Handler (WithError [Set])
getSets = showError . liftIO $ return [minBound..maxBound]


getTypes :: Handler (WithError [CardType])
getTypes = showError . liftIO $ return [minBound..maxBound]


handlerWithError :: Handler (Maybe CardWithTypesAndLinks) -> Handler CardWithTypesAndLinks
handlerWithError hdlr = do
    maybeCard <- hdlr
    case maybeCard of
        Just card -> return card
        Nothing -> throwError $ err404 { errBody = "couldn't find a card of that name" }


showError :: Handler a -> Handler (WithError a)
showError handler =
    catchError (fmap (WithError Nothing) (fmap Just handler)) $ \e ->
        return . flip WithError Nothing . Just . pack $ show (e :: ServerError)


server :: Server PublicAPI
server = getAllCards
            :<|> getFilteredCards
            :<|> getOneCard
            :<|> getSets
            :<|> getTypes


publicAPI :: Proxy PublicAPI
publicAPI = Proxy
