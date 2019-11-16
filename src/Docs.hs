{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Docs where

import CMark (commonmarkToHtml)
import Control.Lens ((&), (^.), (%~), (<>~), mapped, view)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import Data.List.Compat (intercalate, intersperse, sort)
import Data.List.NonEmpty (NonEmpty ((:|)), groupWith)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.String.Conversions (cs)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (pack, toStrict)
import qualified Network.HTTP.Media as M
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Docs
import Servant.Docs.Internal (ToAuthInfo(..), showPath)

import Api (PublicAPI, publicAPI, server, RecoverableQueryParam)
import Database (Card(..))
import Instances
import SubsidiaryTypes


instance ToParam (QueryParams "set" Set) where
    toParam _ =
        DocQueryParam "set"
            ["base", "intrigue-first-ed", "seaside", "dark-ages"]
            ("Name of one or more sets to restrict cards to. Note that we distinguish the first " <>
            "and second editions of the Base set and Intrigue: \"base-first-ed\" returns all " <>
            "cards in the first edition of the Base set, similarly for \"base-second-ed\", while " <>
            "just \"base\" gives back only those cards found in both editions, if that is desired. " <>
            "Multiple sets given together combine as OR (of course), so just include both the first " <>
            "edition and the second if you care about all cards from both editions." )
            List


instance ToParam (RecoverableQueryParam "min-coin-cost" Int) where
    toParam _ =
        DocQueryParam "min-coin-cost"
            ["0", "2", "6", "9"]
            "Restrict cards to those where the coin component of the cost is at least this much."
            Normal


instance ToParam (RecoverableQueryParam "max-coin-cost" Int) where
    toParam _ =
        DocQueryParam "max-coin-cost"
            ["0", "3", "5", "8"]
            "Restrict cards to those where the coin component of the cost is no more than this much."
            Normal


instance ToParam (RecoverableQueryParam "has-potion" Bool) where
    toParam _ =
        DocQueryParam "has-potion"
            ["true", "false"]
            ("Restrict cards to those whose cost either includes, or doesn't include, a potion. " <>
            "(These are only found in the Alchemy set.)")
            Normal


instance ToParam (RecoverableQueryParam "has-debt" Bool) where
    toParam _ =
        DocQueryParam "has-debt"
            ["true", "false"]
            ("Restrict cards to those whose cost either includes, or doesn't include, a debt component. " <>
            "(These are only found in the Empires set.)")
            Normal


instance ToParam (QueryFlag "is-kingdom") where
    toParam _ =
        DocQueryParam "is-kingdom"
            []
            "Restrict cards to kingdom cards only."
            Flag


instance ToParam (RecoverableQueryParam "nonterminal" CanDoItQueryChoice) where
    toParam _ =
        DocQueryParam "nonterminal"
            ["sometimes", "always"]
            ("Restrict cards to those which can, or are guaranteed to, be nonterminal. " <>
            "(That is, have no net cost in actions.) Note that any effects on subsequent turns " <>
            "(eg from Duration cards) are ignored, this only refers to what could happen on " <>
            "the turn the card is played.")
            Normal


instance ToParam (RecoverableQueryParam "village" CanDoItQueryChoice) where
    toParam _ =
        DocQueryParam "village"
            ["sometimes", "always"]
            ("Restrict cards to those which can, or are guaranteed to, result in a net gain of " <>
            "actions. Note that any effects on subsequent turns (eg from Duration cards) are ignored, " <>
            "this only refers to what could happen on the turn the card is played.")
            Normal


instance ToParam (RecoverableQueryParam "no-reduce-hand-size" CanDoItQueryChoice) where
    toParam _ =
        DocQueryParam "no-reduce-hand-size"
            ["sometimes", "always"]
            ("Restrict cards to those which can, or are guaranteed to, lead to no " <>
            "net reduction in hand-size. (Note: no card is \"guaranteed\" to draw cards at " <>
            "all times, because there can be not enough cards left in your deck and discard " <>
            "to draw - these cases are not considered when deciding that a card is will \"always\"" <>
            "fit this category.) Note that any effects on subsequent turns (eg from Duration cards) " <>
            "are ignored, this only refers to what could happen on the turn the card is played.")
            Normal


instance ToParam (RecoverableQueryParam "draws" CanDoItQueryChoice) where
    toParam _ =
        DocQueryParam "draws"
            ["sometimes", "always"]
            ("Restrict cards to those which can, or are guaranteed to, lead to a net " <>
            "increase in hand-size. (Note: no card is \"guaranteed\" to draw cards at " <>
            "all times, because there can be not enough cards left in your deck and discard " <>
            "to draw - these cases are not considered when deciding that a card is will \"always\"" <>
            "fit this category.) Note that any effects on subsequent turns (eg from Duration cards) " <>
            "are ignored, this only refers to what could happen on the turn the card is played.")
            Normal


instance ToParam (QueryFlag "trasher") where
    toParam _ =
        DocQueryParam "trasher"
            []
            ("Restrict cards to those cards which can provide trashing. Note: this does not " <>
            "include cards that just trash themselves, or that trash other players' cards. " <>
            "This is reserved for cards which can lead to thinning or improving one's own deck by " <>
            "removing certain cards and/or replacing them with others. (Note: this includes cards " <>
            "like Ambassador and Island which do not strictly speaking trash, but which remove " <>
            "cards from the deck in other ways. Native Village is not counted because, although it" <>
            "can in theory be used to keep cards out of play, this isn't its primary use.)")
            Flag


instance ToParam (RecoverableQueryParam "extra-buy" CanDoItQueryChoice) where
    toParam _ =
        DocQueryParam "extra-buys"
            ["sometimes", "always"]
            ("Restrict cards to those which can, or are guaranteed to, give an " <>
            "additional Buy when played. Note that any effects on subsequent turns " <>
            "(eg from Duration cards) are ignored, this only refers to what could happen " <>
            "on the turn the card is played.")
            Normal


instance ToParam (QueryParams "type" CardType) where
    toParam _ =
        DocQueryParam "type"
            ["action", "victory", "duration", "prize"]
            ("Restrict cards to those of one or more types. (Multiples can be given, " <>
            "and are combined with OR rather than AND. The only way to find a card which " <>
            "has more than one specific type is to filter on one and then inspect the " <>
            "\"types\" array property.)")
            List


instance ToParam (QueryParams "linked" Text) where
    toParam _ =
        DocQueryParam "linked"
            ["tournament", "spoils", "gladiator", "border-guard"]
            ("Restrict cards to those which are \"linked\" to one or more of the cards whose " <>
            "names you specify here. \"linked\" means they are not normally in play but are if " <>
            "one or more of certain other cards are in play. Examples include all of the Prizes " <>
            "with Tournament, or Ruins with the various Ruin-giving cards in Dark Ages. " <>
            "Split-pile cards are also treated as pairs of linked cards. Note that all links " <>
            "are symmetrical - if A is linked to B then B is automatically linked to A - " <>
            "this has been done to make the relationships easier to think about, even if it " <>
            "sometimes lacks in expressivity.")
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



instance (ToSample a) => ToSample (WithError a) where
    toSamples _ = singleSample . WithError Nothing . snd . head $ toSamples Proxy


apiDocs :: ByteString
apiDocs = fromStrict . encodeUtf8 . fitOnPage . commonmarkToHtml [] . toStrict . pack
            . markdownCustom $ docsWith defaultDocOptions intro extras publicAPI

    where intro = [
            DocIntro "Dominion API - Documentation"
                ["This API allows users to find out information about the many Dominion cards.",
                "Made by Robin Zigmond, developer and occasional (but enthusiastic, "<>
                "although mediocre) Dominion player - please feel " <>
                "free to check out the [source code](https://github.com/robinzigmond/dominion-api) on Github."],
            DocIntro "Types used by this API:"
                ["**Card** - a description of an individual card, as a JSON object. Keys are: \n" <>
                "- `name` (string): the card's name, lower-case with hyphens as word separators\n" <>
                "- `set` (string): the set the card is from. For Base and Intrigue, which have 2 " <>
                "editions with different cards, a `-first-ed` or `-second-ed` suffix is used for " <>
                "cards in only one edition, while those in both editions have no suffix.\n" <>
                "- `coin-cost` (number): the number of coins in the card's cost. Can be " <>
                "null for cards with no cost (eg. Landmarks) - this is different from 0 (eg Copper)\n" <>
                "- `potion-cost` (boolean): indicates whether the cost includes a Potion or not. " <>
                "Can be null for cards with no cost (eg. Landmarks) - this is different from false.\n" <>
                "- `debt-cost` (number): the amount of debt in the card's cost. Can be null for " <>
                "cards with no cost (eg. Landmarks) - this is different from 0.\n" <>
                "- `main-text` (string): the main text on the card, above the dividing line if there is one\n" <>
                "- `other-text` (string): the text below the dividing line on the card. Null if there " <>
                "is no line.\n" <>
                "- `is-kingdom` (boolean): whether the card is a Kingdom card or not\n" <>
                "- `non-terminal` (string): whether the card leaves the number of actions remaining " <>
                "after being played the same as before. Holds one of the three values \"always\", "<>
                "\"sometimes\", or \"never\" - \"sometimes\" meaning the result depends on any " <>
                "factor, including the player's choice, or the contents of players' hands and decks. " <>
                "Is null for non-Action cards, for which this concept makes no sense.\n" <>
                "- `extra-actions` (string): as for `non-terminal` above, but measures the stronger " <>
                "criterion of leaving the player with *more* actions after being played than before.\n" <>
                "- `returns-card` (string): as for `non-terminal` above, but measures hand size rather " <>
                "than action count. Does not account for edge cases like having no cards left to draw, and " <>
                "is likewise null for non-Actions, even though the concept might make sense for some " <>
                "Treasures.\n" <>
                "- `increase-hand-size` (string): as for `extra-actions` above, but measures hand size " <>
                "rather than action count. Does not account for edge cases like having no cards left to " <>
                "draw, and is likewise null for non-Actions, even though the concept might make sense for " <>
                "some Treasures.\n" <>
                "- `extra-buy` (string): as for `non-terminal` above, but refers to whether the card might " <>
                "result in the player having an additional Buy this turn. Unlike the similar properties " <>
                "above, this one is `true` for some Treasures. (But `null` rather than `false` for " <>
                "Treasures which don't give an extra Buy.\n" <>
                "- `trashes` (boolean): whether the card offers any potential to improve the quality of " <>
                " the player's deck - usually by trashing other cards (note that trashing itself, or other " <>
                "players' cards, does not count), but sometimes in other ways such as Ambassador or " <>
                "Island.\n" <>
                "- `types` (array): the list of all types that the card has\n" <>
                "- `linked-cards` (array): the list of all cards (by name) with which the given card is " <>
                "\"linked\". This is a little hard to define precisely, but refers to cards being in " <>
                "play as a result solely of one or other cards also being in play. Examples include " <>
                "the Prizes with Tournament, Spoils with the 3 Dark Ages cards that can let you gain " <>
                "that, and many more in the later expansions.\n\n" <>
                "Also see the descriptions of GET parameters for /cards/filter below, which mostly match " <>
                "up with these keys.\n\n" <>
                "Example of a card representation: \n\n`" <>
                 (cs . encode . snd . head $ toSamples (Proxy :: Proxy CardWithTypesAndLinks)) <>
                 "`"
                ,
                "**Set** - the name of one of the Dominion sets",
                "**Type** - the name of one of the Dominion card types"]
            ]

          fitOnPage = T.replace "<code" "<code style=\"white-space: normal\";"

          extras = extraForAllCards <> extraForFilters <> extraForOneCard
                    <> extraForSets <> extraForTypes

          extraForAllCards :: ExtraInfo PublicAPI
          extraForAllCards =
            extraInfo (Proxy :: Proxy ("cards" :> Get '[JSON] (WithError [CardWithTypesAndLinks]))) $
                defAction & notes <>~ [DocNote "Returns"
                    ["Array of cards.", "Convenience endpoint to get a list of *all* cards, if desired."]]

          extraForFilters :: ExtraInfo PublicAPI
          extraForFilters =
            extraInfo (Proxy :: Proxy (
                "cards" :> "filter" :> QueryParams "set" Set
                        :> RecoverableQueryParam "min-coin-cost" Int
                        :> RecoverableQueryParam "max-coin-cost" Int
                        :> RecoverableQueryParam "has-potion" Bool
                        :> RecoverableQueryParam "has-debt" Bool :> QueryFlag "is-kingdom"
                        :> RecoverableQueryParam "nonterminal" CanDoItQueryChoice
                        :> RecoverableQueryParam "village" CanDoItQueryChoice
                        :> RecoverableQueryParam "no-reduce-hand-size" CanDoItQueryChoice
                        :> RecoverableQueryParam "draws" CanDoItQueryChoice :> QueryFlag "trasher"
                        :> RecoverableQueryParam "extra-buy" CanDoItQueryChoice
                        :> QueryParams "type" CardType :> QueryParams "linked" Text
                        :> Get '[JSON] (WithError [CardWithTypesAndLinks])
            )) $ defAction & notes <>~ [DocNote "Returns"
                ["Array of cards (possibly empty) which satisfy all specified filters."]]

          extraForOneCard :: ExtraInfo PublicAPI
          extraForOneCard =
            extraInfo (Proxy :: Proxy ("cards" :> Capture "card-name" Text
                    :> Get '[JSON] (WithError CardWithTypesAndLinks))) $
                defAction & notes <>~ [DocNote "Returns"
                    ["Single card, that whose name is :card-name",
                        "Returns an error if there is no such card"]]

          extraForSets :: ExtraInfo PublicAPI
          extraForSets =
            extraInfo (Proxy :: Proxy ("sets" :> Get '[JSON] (WithError [Set]))) $
                defAction & notes <>~ [DocNote "Returns"
                    ["List of all set names, in order of release (ending with \"promos\", " <>
                    "which is not a real set but a gathering of all individual promotional cards).",
                    "This is a convenience endpoint provided for developers who desire an " <>
                    "up-to-date list of sets, for example to allow users of an application to " <>
                    "choose which sets they own."]]

          extraForTypes :: ExtraInfo PublicAPI
          extraForTypes =
            extraInfo (Proxy :: Proxy ("types" :> Get '[JSON] (WithError [CardType]))) $
                defAction & notes <>~ [DocNote "Returns"
                    ["List of all different card types which exist",
                    "This is a convenience endpoint provided for developers of applications " <>
                    "which need an up-to-date list of all types."]]

-- my own custom version of the markdown function from the original Servant.Docs.Internal module,
-- which I started from but have edited to fit my needs better, mainly to remove irrelevant info
markdownCustom :: API -> String
markdownCustom api = unlines $
        introsStr (api ^. apiIntros)
    ++ (concatMap (uncurry printEndpoint) . sort . HM.toList $ api ^. apiEndpoints)

    where
        printEndpoint :: Endpoint -> Action -> [String]
        printEndpoint endpoint action =
            str :
            "" :
            notesStr (action ^. notes) ++
            authStr (action ^. authInfo) ++
            capturesStr (action ^. captures) ++
            paramsStr meth (action ^. params) ++
            rqbodyStr (action ^. rqtypes) (action ^. rqbody) ++
            --responseStr (action ^. response) ++
            []

            where str = "## " ++ BSC.unpack meth
                    ++ " " ++ showPath (endpoint^.path)

                  meth = endpoint ^. method

        introsStr :: [DocIntro] -> [String]
        introsStr [] = []
        introsStr (x:xs) = firstIntroStr x ++ subsidiaryIntros xs

        subsidiaryIntros :: [DocIntro] -> [String]
        subsidiaryIntros = concatMap introStr

        firstIntroStr :: DocIntro -> [String]
        firstIntroStr i =
            ("# " ++ i ^. introTitle) :
            "" :
            intersperse "" (i ^. introBody) ++
            "" :
            []

        introStr :: DocIntro -> [String]
        introStr i =
            ("### " ++ i ^. introTitle) :
            "" :
            intersperse "" (i ^. introBody) ++
            "" :
                []

        notesStr :: [DocNote] -> [String]
        notesStr = addHeading
                    . concatMap noteStr
            where
            addHeading nts = maybe nts (\hd -> ("### " ++ hd) : "" : nts) Nothing

        noteStr :: DocNote -> [String]
        noteStr nt =
            (hdr ++ nt ^. noteTitle) :
            "" :
            intersperse "" (nt ^. noteBody) ++
            "" :
            []
            where
            hdr = "### "

        authStr :: [DocAuthentication] -> [String]
        authStr [] = []
        authStr auths =
            let authIntros = mapped %~ view authIntro $ auths
                clientInfos = mapped %~ view authDataRequired $ auths
            in "### Authentication":
                "":
                unlines authIntros :
                "":
                "Clients must supply the following data" :
                unlines clientInfos :
                "" :
                []

        capturesStr :: [DocCapture] -> [String]
        capturesStr [] = []
        capturesStr l =
            "### Captures:" :
            "" :
            map captureStr l ++
            "" :
            []

        captureStr cap =
            "- *" ++ (cap ^. capSymbol) ++ "*: " ++ (cap ^. capDesc)

        headersStr :: [Text] -> [String]
        headersStr [] = []
        headersStr l =
            "### Headers:" :
            "" :
            map headerStr l ++
            "" :
            []

            where headerStr hname = "- This endpoint is sensitive to the value of the **"
                                ++ unpack hname ++ "** HTTP header."

        paramsStr :: Method -> [DocQueryParam] -> [String]
        paramsStr _ [] = []
        paramsStr m l =
            ("### " ++ cs m ++ " Parameters:") :
            "" :
            map (paramStr m) l ++
            "" :
            []

        paramStr m param = unlines $
            ("- " ++ param ^. paramName) :
            (if (not (null values) || param ^. paramKind /= Flag)
            then ["     - **Example Values**: *" ++ intercalate ", " values ++ "*"]
            else []) ++
            ("     - **Description**: " ++ param ^. paramDesc) :
            (if (param ^. paramKind == List)
            then ["     - This parameter is a **list**. Multiple " ++ cs m ++ " parameters with the name *"
                    ++ param ^. paramName ++ "* can be included and all will be taken into account."]
            else []) ++
            (if (param ^. paramKind == Flag)
            then ["     - This parameter is a **flag**. This means no value is expected to be " <>
            "associated to this parameter - all that matters is whether the parameter is included."]
            else []) ++
            []

            where values = param ^. paramValues

        rqbodyStr :: [M.MediaType] -> [(Text, M.MediaType, ByteString)]-> [String]
        rqbodyStr [] [] = []
        rqbodyStr types s =
            ["### Request:", ""]
            <> formatTypes types
            <> formatBodies FirstContentType s

        formatTypes [] = []
        formatTypes ts = ["- Supported content types are:", ""]
            <> map (\t -> "    - `" <> show t <> "`") ts
            <> [""]

        -- This assumes that when the bodies are created, identical
        -- labels and representations are located next to each other.
        formatBodies :: ShowContentTypes -> [(Text, M.MediaType, ByteString)] -> [String]
        formatBodies ex bds = concatMap formatBody (select bodyGroups)
            where
            bodyGroups :: [(Text, NonEmpty M.MediaType, ByteString)]
            bodyGroups =
                map (\grps -> let (t,_,b) = NE.head grps in (t, fmap (\(_,m,_) -> m) grps, b))
                . groupWith (\(t,_,b) -> (t,b))
                $ bds

            select = case ex of
                        AllContentTypes  -> id
                        FirstContentType -> map (\(t,ms,b) -> (t, NE.head ms :| [], b))

        formatBody :: (Text, NonEmpty M.MediaType, ByteString) -> [String]
        formatBody (t, ms, b) =
            "- " <> title <> " (" <> mediaList ms <> "):" :
            contentStr (NE.head ms) b
            where
            mediaList = fold . NE.intersperse ", " . fmap (\m -> "`" ++ show m ++ "`")

            title
                | T.null t  = "Example"
                | otherwise = cs t

        markdownForType mime_type =
            case (M.mainType mime_type, M.subType mime_type) of
                ("text", "html") -> "html"
                ("application", "xml") -> "xml"
                ("text", "xml") -> "xml"
                ("application", "json") -> "javascript"
                ("application", "javascript") -> "javascript"
                ("text", "css") -> "css"
                (_, _) -> ""


        contentStr mime_type body =
            "" :
            "```" <> markdownForType mime_type :
            cs body :
            "```" :
            "" :
            []

        responseStr :: Servant.Docs.Response -> [String]
        responseStr resp =
            "### Response:" :
            bodies

            where bodies = case resp ^. respBody of
                    []        -> ["- No response body\n"]
                    [("", t, r)] -> "- Response body as below." : contentStr t r
                    xs        ->
                        formatBodies AllContentTypes xs


type PublicAPIWithDocs = PublicAPI :<|> "docs" :> Raw


publicAPIWithDocs :: Proxy PublicAPIWithDocs
publicAPIWithDocs = Proxy


server :: Server PublicAPIWithDocs
server = Api.server :<|> Tagged serveDocs
    where
        serveDocs _ respond =
            respond $ responseLBS ok200 [("Content-Type", "text/html")] apiDocs

