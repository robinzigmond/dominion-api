{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SubsidiaryTypes where

import Data.Aeson
import Data.Text (pack, unpack)
import Database.Persist.TH
import GHC.Generics
import Servant (FromHttpApiData(..))
import Text.Casing (kebab, pascal)
import Text.Read (readMaybe)
import Web.HttpApiData (readTextData)


data CardType = Action | Treasure | Victory | Curse | Attack | Reaction | Duration
                | Prize | Shelter | Ruins | Looter | Knight | Reserve | Traveller
                | Gathering | Castle | Night | Heirloom | Fate | Doom | Spirit
                | Zombie | Event | Landmark | Boon | Hex | State | Artifact
                | Project
                deriving (Eq, Show, Read, Generic)

instance ToJSON CardType where
    toJSON = String . pack . kebab . show

instance FromJSON CardType where
    parseJSON (String s) = case (readMaybe . pascal . unpack $ s :: Maybe CardType) of
        Just c -> return c
    parseJSON _ = fail "invalid type value given"

instance FromHttpApiData CardType where
    parseQueryParam = readTextData . pack . pascal . unpack

derivePersistField "CardType"


data Set = Base | BaseFirstEd | BaseSecondEd | Intrigue | IntrigueFirstEd
            | IntrigueSecondEd | Seaside | Alchemy | Prosperity
            | Cornucopia | Hinterlands | DarkAges | Guilds
            | Adventures | Empires | Nocturne | Renaissance | Promo
            deriving (Eq, Show, Read, Generic)

instance ToJSON Set where
    toJSON = String . pack . kebab . show

instance FromJSON Set where
    parseJSON (String s) = case (readMaybe . pascal . unpack $ s :: Maybe Set) of
        Just c -> return c
    parseJSON _ = fail "invalid set value given"

instance FromHttpApiData Set where
    parseQueryParam = readTextData . pack . pascal . unpack

derivePersistField "Set"


data CanDoIt = Always | Sometimes | Never deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON CanDoIt where
    toJSON = String . pack . kebab . show

instance FromJSON CanDoIt where
    parseJSON (String s) = case (readMaybe . pascal . unpack $ s :: Maybe CanDoIt) of
        Just c -> return c
    parseJSON _ = fail "invalid value - must be always, sometimes or never"
 
derivePersistField "CanDoIt"
