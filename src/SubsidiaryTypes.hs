{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SubsidiaryTypes where

import Data.Aeson
import Data.Char (toLower)
import Data.Text (pack)
import Database.Persist.TH
import GHC.Generics
import Servant (FromHttpApiData(..))
import Web.HttpApiData (readTextData)


data CardType = Action | Treasure | Victory | Curse | Attack | Reaction | Duration
                | Prize | Shelter | Ruins | Looter | Knight | Reserve | Traveller
                | Gathering | Castle | Night | Heirloom | Fate | Doom | Spirit
                | Zombie | Event | Landmark | Boon | Hex | State | Artifact
                | Project
                deriving (Eq, Show, Read, Generic)

instance ToJSON CardType
instance FromJSON CardType

instance FromHttpApiData CardType where
    parseQueryParam = readTextData

derivePersistField "CardType"


data Set = Base | BaseFirstEd | BaseSecondEd | Intrigue | IntrigueFirstEd
            | IntrigueSecondEd | Seaside | Alchemy | Prosperity
            | Cornucopia | Hinterlands | DarkAges | Guilds
            | Adventures | Empires | Nocturne | Renaissance | Promo
            deriving (Eq, Show, Read, Generic)

instance ToJSON Set
instance FromJSON Set

instance FromHttpApiData Set where
    parseQueryParam = readTextData

derivePersistField "Set"



data CanDoIt = Always | Sometimes | Never deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON CanDoIt where
    toJSON = String . pack . map toLower . show

instance FromJSON CanDoIt where
    parseJSON (String "always") = return Always
    parseJSON (String "sometimes") = return Sometimes
    parseJSON (String "never") = return Never
    parseJSON _ = fail "invalid value - must be always, sometimes or never"
 

derivePersistField "CanDoIt"
