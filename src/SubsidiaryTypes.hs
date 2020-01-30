{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SubsidiaryTypes where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text (pack, unpack, append, Text)
import Database.Persist.TH
import GHC.Generics
import Servant (FromHttpApiData(..), ToHttpApiData(..))
import Text.Casing (kebab, pascal)
import Text.Read (readMaybe)
import Web.HttpApiData (readTextData)


data CardType = Action | Treasure | Victory | Curse | Attack | Reaction | Duration
                | Prize | Shelter | Ruins | Looter | Knight | Reserve | Traveller
                | Gathering | Castle | Night | Heirloom | Fate | Doom | Spirit
                | Zombie | Event | Landmark | Boon | Hex | State | Artifact
                | Project
                deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

derivePersistField "CardType"


instance ToJSON CardType where
    toJSON = String . pack . kebab . show


instance FromJSON CardType where
    parseJSON (String s) = case (readMaybe . pascal . unpack $ s :: Maybe CardType) of
        Just c -> return c
    parseJSON _ = fail "invalid type value given"


instance FromHttpApiData CardType where
    parseQueryParam = readTextData . pack . pascal . unpack


instance ToHttpApiData CardType where
    toQueryParam = pack . show


data Set = Base | BaseFirstEd | BaseSecondEd | Intrigue | IntrigueFirstEd
            | IntrigueSecondEd | Seaside | Alchemy | Prosperity
            | Cornucopia | Hinterlands | DarkAges | Guilds
            | Adventures | Empires | Nocturne | Renaissance | Promo
            deriving (Eq, Show, Read, Enum, Bounded, Generic)

derivePersistField "Set"


instance ToJSON Set where
    toJSON = String . pack . kebab . show


instance FromJSON Set where
    parseJSON (String s) = case (readMaybe . pascal . unpack $ s :: Maybe Set) of
        Just c -> return c
    parseJSON _ = fail "invalid set value given"


instance FromHttpApiData Set where
    parseQueryParam = readTextData . pack . pascal . unpack


instance ToHttpApiData Set where
    toQueryParam = pack . show


data CanDoIt = Always | Sometimes | Never deriving (Eq, Ord, Show, Read, Generic)

derivePersistField "CanDoIt"


instance ToJSON CanDoIt where
    toJSON = String . pack . kebab . show


instance FromJSON CanDoIt where
    parseJSON (String s) = case (readMaybe . pascal . unpack $ s :: Maybe CanDoIt) of
        Just c -> return c
    parseJSON _ = fail "invalid value - must be always, sometimes or never"


data CanDoItQueryChoice = CanSometimes | CanAlways deriving (Read, Show, Enum)


instance FromHttpApiData CanDoItQueryChoice where
    parseQueryParam "sometimes" = Right CanSometimes
    parseQueryParam "always" = Right CanAlways
    parseQueryParam x = Left $ Data.Text.append "invalid parameter for filter: " x


instance ToHttpApiData CanDoItQueryChoice where
    toQueryParam = pack . show


possibleChoices :: CanDoItQueryChoice -> [CanDoIt]
possibleChoices CanSometimes = [Always, Sometimes]
possibleChoices CanAlways = [Always]


data WithError a = WithError {error :: Maybe Text, result :: Maybe a} deriving (Show, Generic)


instance (ToJSON a) => ToJSON (WithError a) where
    toJSON (WithError Nothing r) = toJSON r
    toJSON (WithError (Just e) _) = object ["error" .= e]


instance (FromJSON a) => FromJSON (WithError a) where
    parseJSON v@(Object o)
        = (WithError <$> (Just <$> (o .: "error")) <*> pure Nothing)
            <|> (WithError <$> pure Nothing <*> parseJSON v)
    parseJSON v = WithError <$> pure Nothing <*> parseJSON v
