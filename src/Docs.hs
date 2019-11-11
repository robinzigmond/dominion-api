{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Docs where

import CMark (commonmarkToHtml)
import Control.Lens ((^.), (%~), mapped, view)
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
apiDocs = fromStrict . encodeUtf8 . fitOnPage . commonmarkToHtml [] . toStrict . pack
            . markdownCustom $ docsWithIntros [intro] dominionAPI

    where intro = DocIntro "Dominion API - Documentation"
            ["This API allows users to find out information about the many Dominion cards."]
          fitOnPage = T.replace "<code" "<code style=\"white-space: normal\";"


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
            responseStr (action ^. response) ++
            []

            where str = "## " ++ BSC.unpack meth
                    ++ " " ++ showPath (endpoint^.path)

                  meth = endpoint ^. method

        introsStr :: [DocIntro] -> [String]
        introsStr = concatMap introStr

        introStr :: DocIntro -> [String]
        introStr i =
            ("# " ++ i ^. introTitle) :
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
            then ["     - This parameter is a **list**. All " ++ cs m ++ " parameters with the name *"
                    ++ param ^. paramName ++ "* will forward their values in a list to the handler."]
            else []) ++
            (if (param ^. paramKind == Flag)
            then ["     - This parameter is a **flag**. This means no value is expected to be associated to this parameter."]
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


type DominionAPIWithDocs = DominionAPI :<|> Raw


dominionAPIWithDocs :: Proxy DominionAPIWithDocs
dominionAPIWithDocs = Proxy


server :: Server DominionAPIWithDocs
server = Api.server :<|> Tagged serveDocs
    where
        serveDocs _ respond =
            respond $ responseLBS ok200 [("Content-Type", "text/html")] apiDocs


api :: Application
api = serveWithContext dominionAPIWithDocs authContext Docs.server
