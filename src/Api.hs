{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.Error.Class (catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B (pack, unpack)
import qualified Data.Set as S (toList, fromList)
import Data.Text hiding (map, null, foldr)
import Database.Esqueleto
import Servant
import Web.HttpApiData (LenientData(..))

import Database
import Instances
import SubsidiaryTypes


{- taken from Alp Mestanogullari's Stack Overflow answer here:
   https://stackoverflow.com/questions/51146315/servant-queryparams-parse-error
   Not sure why Servant by default makes it impossible to recover from query param parsing failures,
   or why the documentation seems to contain no word of this relatively simple way to fix it -}
type RecoverableQueryParam = QueryParam' '[Optional, Lenient]

type RecoverableQueryParams s a = QueryParams s (LenientData a)


type PublicAPI = "cards" :> RecoverableQueryParams "set" Set
                        :> RecoverableQueryParam "min-coin-cost" Int
                        :> RecoverableQueryParam "max-coin-cost" Int
                        :> RecoverableQueryParam "has-potion" Bool
                        :> RecoverableQueryParam "has-debt" Bool :> QueryFlag "is-kingdom"
                        :> RecoverableQueryParam "nonterminal" CanDoItQueryChoice
                        :> RecoverableQueryParam "village" CanDoItQueryChoice
                        :> RecoverableQueryParam "no-reduce-hand-size" CanDoItQueryChoice
                        :> RecoverableQueryParam "draws" CanDoItQueryChoice :> QueryFlag "trasher"
                        :> RecoverableQueryParam "extra-buy" CanDoItQueryChoice
                        :> RecoverableQueryParams "type" CardType
                        :> QueryParams "linked" Text
                        :> Get '[JSON] (WithError [CardWithTypesAndLinks])
                    :<|> "card" :> Capture "card-name" Text
                        :> Get '[JSON] (WithError CardWithTypesAndLinks)
                    :<|> "sets" :> Get '[JSON] (WithError [Set])
                    :<|> "types" :> Get '[JSON] (WithError [CardType])



getOneCard :: RunDB (Maybe CardWithTypesAndLinks) -> Text -> Handler (WithError CardWithTypesAndLinks)
getOneCard runDB name = showError . handlerWithError . liftIO . runDB $ do
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


getFilteredCards :: RunDB [CardWithTypesAndLinks] -> [LenientData Set] -> Maybe (Either Text Int)
                        -> Maybe (Either Text Int)
                        -> Maybe (Either Text Bool) -> Maybe (Either Text Bool)
                        -> Bool -> Maybe (Either Text CanDoItQueryChoice)
                        -> Maybe (Either Text CanDoItQueryChoice)
                        -> Maybe (Either Text CanDoItQueryChoice)
                        -> Maybe (Either Text CanDoItQueryChoice)
                        -> Bool -> Maybe (Either Text CanDoItQueryChoice)
                        -> [LenientData CardType] -> [Text]
                        -> Handler (WithError [CardWithTypesAndLinks])
getFilteredCards runDB sets maybeMinCost maybeMaxCost maybeNeedsPotion maybeNeedsDebt
        mustBeKingdom maybeNonTerminal maybeVillage maybeNoHandsizeReduction
        maybeDraws mustTrash maybeExtraBuy types links
        = showError . checkForError . liftIO . runDB $ do
            sqlres <- select $
                from $ \(c `InnerJoin` tc `InnerJoin` t `LeftOuterJoin` lp `LeftOuterJoin` c1
                        `FullOuterJoin` lp1 `FullOuterJoin` c2 `InnerJoin` tc1 `InnerJoin` t1) -> do
                    let actualSets = extract sets
                    let actualTypes = extract types
                    let filloutBase = if BaseFirstEd `elem` actualSets 
                            || BaseSecondEd `elem` actualSets
                        then Base:actualSets
                        else actualSets
                    let allSets = if IntrigueFirstEd `elem` actualSets
                            || IntrigueSecondEd `elem` actualSets
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
                                        else [t ^. TypeName `in_` valList actualTypes]
                    let linksQuery = if null links
                                        then []
                                        else [c1 ?. CardName `in_` valList (map Just links)]
                    let queries = setsQuery ++ minCostQuery ++ maxCostQuery ++ potionQuery
                            ++ debtQuery ++ kingdomQuery ++ nonTerminalQuery
                            ++ villageQuery ++ noHandSizeReductionQuery ++ drawsQuery
                            ++ trashQuery ++ extraBuyQuery ++ typesQuery ++ linksQuery
                    {- this is breaking when no filters are applied at all, because when there is no WHERE
                    clause in the query then we get results with no matching "c" fields. As soon as any
                    restriction is placed on this then obviously the NULL results disappear and the
                    query is OK. The solution is simple, if "dirty" (would have been better to rewrite
                    the query to return a Maybe value for c and handle that, but I couldn't get it
                    to compile...) - if there would be no queries, insert a dummy one which has no
                    effect other than to remove nulls. Eg. the one used below -}
                    let amendedQueries =
                            if null queries
                                then [c ^. CardSet `in_` valList [minBound..maxBound]]
                                else queries
                    forM_ amendedQueries where_
                    on (t1 ^. TypeId ==. tc1 ^. TypeCardTypeId)
                    on (c ^. CardId ==. tc1 ^. TypeCardCardId)
                    on (lp1 ?. LinkPairsCardTwo ==. c2 ?. CardId)
                    on (lp1 ?. LinkPairsCardOne ==. lp ?. LinkPairsCardOne)
                    on (c1 ?. CardId ==. lp ?. LinkPairsCardTwo)
                    on (lp ?. LinkPairsCardOne ==. just (c ^. CardId))
                    on (t ^. TypeId ==. tc ^. TypeCardTypeId)
                    on (c ^. CardId ==. tc ^. TypeCardCardId)
                    return (c, t1, c2)
            return . map uniques $ foldr mergeList [] sqlres
                where
                    extract :: [LenientData a] -> [a]
                    extract [] = []
                    extract (LenientData (Right a) : as) = a : extract as
                    extract (_:as) = extract as

                    checkForError act = case checkQueryErrors [maybeMinCost, maybeMaxCost]
                            <|> checkQueryErrors [maybeNeedsPotion, maybeNeedsDebt]
                            <|> checkQueryErrors [maybeNonTerminal, maybeVillage, maybeNoHandsizeReduction,
                                maybeDraws, maybeExtraBuy]
                            <|> checkQueryErrors (Just . getLenientData <$> sets)
                            <|> checkQueryErrors (Just . getLenientData <$> types) of
                        Just e -> throwQueryError . B.pack $ unpack e
                        Nothing -> act

                    checkQueryErrors :: [Maybe (Either e a)] -> Maybe e
                    checkQueryErrors [] = Nothing
                    checkQueryErrors (Just (Left e):xs) = Just e
                    checkQueryErrors (_:xs) = checkQueryErrors xs

                    throwQueryError e = throwError $ err422 { errBody = e }


uniques :: CardWithTypesAndLinks -> CardWithTypesAndLinks
uniques (CardWithTypesAndLinks c ts ls) = CardWithTypesAndLinks c (noRepeats ts) (noRepeats ls)


noRepeats :: (Ord a) => [a] -> [a]
noRepeats = S.toList . S.fromList


resolveCard :: Entity Card -> Entity Type -> Maybe (Entity Card) -> CardWithTypesAndLinks
resolveCard c t (Just c1) = CardWithTypesAndLinks (entityVal c) [typeName . entityVal $ t]
    [cardName . entityVal $ c1]
resolveCard c t Nothing = CardWithTypesAndLinks (entityVal c) [typeName . entityVal $ t] [] 


mergeList :: (Entity Card, Entity Type, Maybe (Entity Card))
    -> [CardWithTypesAndLinks] -> [CardWithTypesAndLinks]
mergeList (c, t, c1) [] = [resolveCard c t c1]
mergeList (c, t, c1) (cd@(CardWithTypesAndLinks card _ _) : cs)
    | cardName (entityVal c) == cardName card
        = mergeCards (t, c1) cd : cs
    | otherwise = cd : mergeList (c, t, c1) cs


mergeCards :: (Entity Type, Maybe (Entity Card)) -> CardWithTypesAndLinks -> CardWithTypesAndLinks
mergeCards (t, Just c1) (CardWithTypesAndLinks card types links)
    = CardWithTypesAndLinks card (typeName (entityVal  t) : types)
        (cardName (entityVal c1) : links)
mergeCards (t, Nothing) (CardWithTypesAndLinks card types links)
    = CardWithTypesAndLinks card (typeName (entityVal  t) : types) links


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
    catchError (fmap (WithError Nothing) (fmap Just handler)) $
        return . flip WithError Nothing . Just . pack . B.unpack . errBody


server :: (forall a. RunDB a) -> Server PublicAPI
server runDB = getFilteredCards runDB
                :<|> getOneCard runDB
                :<|> getSets
                :<|> getTypes


publicAPI :: Proxy PublicAPI
publicAPI = Proxy
