{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Docs where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Docs
import Servant.Docs.Internal (ToAuthInfo(..))

import Api (DominionAPI, dominionAPI, server, authContext)
import Database (Card(..))
import Instances
import SubsidiaryTypes


instance ToParam (QueryParams "set" Set) where
    toParam _ =
        DocQueryParam "set"
            ["base", "intrigue-first-ed", "seaside", "dark-ages"]
            "Name of one or more sets to restrict cards to."
            List


instance ToParam (QueryParam "min-coin-cost" Int) where
    toParam _ =
        DocQueryParam "min-coin-cost"
            ["0", "2", "6", "9"]
            "Restrict cards to those where the coin component of the cost is at least this much."
            Normal


instance ToParam (QueryParam "max-coin-cost" Int) where
    toParam _ =
        DocQueryParam "max-coin-cost"
            ["0", "2", "6", "9"]
            "Restrict cards to those where the coin component of the cost is no more than this much."
            Normal


instance ToParam (QueryParam "has-potion" Bool) where
    toParam _ =
        DocQueryParam "has-potion"
            ["true", "false"]
            "Restrict cards to those whose cost either includes, or doesn't include, a potion."
            Normal


instance ToParam (QueryParam "has-debt" Bool) where
    toParam _ =
        DocQueryParam "has-debt"
            ["true", "false"]
            "Restrict cards to those whose cost either includes, or doesn't include, a debt component."
            Normal


instance ToParam (QueryFlag "is-kingdom") where
    toParam _ =
        DocQueryParam "is-kingdom"
            []
            "Restrict cards to kingdom cards only."
            Flag


instance ToParam (QueryParam "nonterminal" CanDoItQueryChoice) where
    toParam _ =
        DocQueryParam "nonterminal"
            ["sometimes", "always"]
            "Restrict cards to kingdom cards which can, or are guaranteed to, be nonterminal. (That is, have no net cost in actions.)"
            Normal


instance ToParam (QueryParam "village" CanDoItQueryChoice) where
    toParam _ =
        DocQueryParam "village"
            ["sometimes", "always"]
            "Restrict cards to kingdom cards which can, or are guaranteed to, result in a net gain of actions."
            Normal


instance ToParam (QueryParam "no-reduce-hand-size" CanDoItQueryChoice) where
    toParam _ =
        DocQueryParam "no-reduce-hand-size"
            ["sometimes", "always"]
            "Restrict cards to kingdom cards which can, or are guaranteed to, lead to no net reduction in hand-size. (Note: \"guaranteed\" excludes the case where there are not enough cards left in your deck and discard to draw.)"
            Normal


instance ToParam (QueryParam "draws" CanDoItQueryChoice) where
    toParam _ =
        DocQueryParam "draws"
            ["sometimes", "always"]
            "Restrict cards to kingdom cards which can, or are guaranteed to, lead to a net increase in hand-size. (Note: \"guaranteed\" excludes the case where there are not enough cards left in your deck and discard to draw.)"
            Normal


instance ToParam (QueryFlag "trasher") where
    toParam _ =
        DocQueryParam "trasher"
            []
            "Restrict cards to those cards which can provide trashing. Note: this does not inclde cards that just trash themselves, or that trash other players' cards. This is just for cards which can lead to thinning or improving one's own deck by removing certain cards and/or replacing them with others. (Note: this includes cards like Ambassador and Island which do not strictly speaking trash, but which removes cards from the deck in other ways."
            Flag


instance ToParam (QueryParam "extra-buy" CanDoItQueryChoice) where
    toParam _ =
        DocQueryParam "extra-buys"
            ["sometimes", "always"]
            "Restrict cards to kingdom cards which can, or are guaranteed to, give an additional Buy when played."
            Normal


instance ToParam (QueryParams "type" CardType) where
    toParam _ =
        DocQueryParam "type"
            ["action", "victory", "duration", "prize"]
            "Restrict cards to those of one or more types. (Multiples can be given, and are combined with OR rather than AND."
            List


instance ToParam (QueryParams "linked" Text) where
    toParam _ =
        DocQueryParam "linked"
            ["tournament", "spoils", "gladiator", "border-guard"]
            "Restrict cards to those which are \"linked\" to one or more of the cards whose names you specify here. \"linked\" means they are not normally in play but are if one or more of certain other cards are in play. Examples include all of the Prizes with Tournament, or Ruins with the various Ruin-giving cards in Dark Ages. Split-pile cards are also treated as pairs of linked cards. Note that all links are symmetrical - if A is linked to B then B is automatically linked to A - this has been done to make the relationships easier to think about, even if it sometimes lacks in expressivity."
            List


instance ToCapture (Capture "card-name" Text) where
    toCapture _ =
        DocCapture "card-name" "(string) name of the card - in lowercase, with words separated by hyphens"


instance ToSample CardWithTypesAndLinks where
    toSamples _ = singleSample $ CardWithTypesAndLinks
        (Card "village" Base (Just 3) (Just False) (Just 0)
            "+1 Card. +2 Actions." Nothing True (Just Always)
            (Just Always) (Just Always) (Just Never) (Just Never) False)
        [Action] []


instance ToSample Set where
    toSamples _ = singleSample Prosperity


instance ToSample CardType where
    toSamples _ = singleSample Reaction


instance ToAuthInfo (BasicAuth "dominion" ()) where
    toAuthInfo _ = DocAuthentication "" "" -- dummy instance for now, so it compiles
    -- ultimately I don't want these "private" routes documented at all!


apiDocs :: ByteString
apiDocs = encodeUtf8
            . pack
            . markdown
            $ docsWithIntros [intro] dominionAPI

  where intro = DocIntro "Dominion API"
            ["This API allows users to find out information about the many Dominion cards."]


type DominionAPIWithDocs = DominionAPI :<|> Raw


dominionAPIWithDocs :: Proxy DominionAPIWithDocs
dominionAPIWithDocs = Proxy


server :: Server DominionAPIWithDocs
server = Api.server :<|> Tagged serveDocs
    where
        serveDocs _ respond =
            respond $ responseLBS ok200 [("Content-Type", "text/plain")] apiDocs


api :: Application
api = serveWithContext dominionAPIWithDocs authContext Docs.server
