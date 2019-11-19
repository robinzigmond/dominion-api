{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Instances where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text
import GHC.Generics

import SubsidiaryTypes
import Database(Card(..))


-- custom FromJSON and ToJSON instances. These are mostly similar to the ones Persistent would have
-- auto-generated, but have important improvements:
-- 1) using more user-friendly names for certain fields and values
-- 2) allowing some fields to take default values under certain conditions
instance FromJSON Card where
    parseJSON (Object v) =
        Card
        <$> v .: "name"
        <*> v .: "set"
        <*> v .:? "coin-cost"
        <*> potionCost
        <*> debtCost
        <*> v .: "main-text"
        <*> v .:? "other-text"
        <*> v .: "is-kingdom"
        <*> v .:? "non-terminal"
        <*> v .:? "extra-actions"
        <*> v .:? "returns-card"
        <*> v .:? "increase-hand-size"
        <*> v .:? "extra-buy"
        <*> v .: "trashes"
        where potionCost = do
                maybePotion <- v .:? "potion-cost" :: Parser (Maybe Bool)
                maybeCoin <- v .:? "coin-cost" :: Parser (Maybe Int)
                return $ if maybePotion == Nothing
                            then if maybeCoin == Nothing
                                then Nothing
                                else Just False
                            else maybePotion

              debtCost = do
                maybeDebt <- v .:? "debt-cost" :: Parser (Maybe Int)
                maybeCoin <- v .:? "coin-cost" :: Parser (Maybe Int)
                return $ if maybeDebt == Nothing
                            then if maybeCoin == Nothing
                                then Nothing
                                else Just 0
                            else maybeDebt


instance ToJSON Card where
    toJSON (Card name set coinCost potionCost debtCost mainText otherText isKingdom nonTerminal
            extraActions returnsCard increaseHandSize extraBuy trashes)
        = object ["name" .= name,
                  "set" .= set,
                  "coin-cost" .= coinCost,
                  "potion-cost" .= potionCost,
                  "debt-cost" .= debtCost,
                  "main-text" .= mainText,
                  "other-text" .= otherText,
                  "is-kingdom" .= isKingdom,
                  "non-terminal" .= nonTerminal,
                  "extra-actions" .= extraActions,
                  "returns-card" .= returnsCard,
                  "increase-hand-size" .= increaseHandSize,
                  "extra-buy" .= extraBuy,
                  "trashes" .= trashes
                  ]


data CardWithTypesAndLinks = CardWithTypesAndLinks Card [CardType] [Text] deriving (Eq, Show, Generic)


instance FromJSON CardWithTypesAndLinks where
    parseJSON (Object v) =
        CardWithTypesAndLinks <$> parsedCard <*> v .: "types"
            <*> fmap (fromMaybe []) (v .:? "linked-cards")
        where parsedCard = parseJSON (Object v)  


instance ToJSON CardWithTypesAndLinks where
    toJSON (CardWithTypesAndLinks card types linked)
        = Object $ HM.insert "types" (toJSON types)
            $ HM.insert "linked-cards" (toJSON linked) (fromObject $ toJSON card)
        where fromObject (Object obj) = obj
              fromObject _ = HM.empty -- can't happen, but a "sensible" default in case of madness!