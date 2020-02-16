{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types (Status(..))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import System.Directory (removeFile)
import System.IO
import Test.Hspec
import Web.HttpApiData (LenientData(..))

import Complete (api, DominionAPI)
import Database (runTestDB, Card(..), readEnv)
import Instances (CardWithTypesAndLinks(..))
import qualified SubsidiaryTypes as S


withApi :: IO () -> IO ()
withApi action =
    -- we can spin up a server in another thread and kill that thread when done
    -- in an exception-safe way
    bracket (forkIO . run 8888 $ api runTestDB) killThread (const action)


spec :: Spec
spec = apiTestRequestsSpec


apiTestRequestsSpec :: Spec
apiTestRequestsSpec = around_ withApi $ do
    let private :<|> public :<|> docs = client (Proxy :: Proxy DominionAPI)
    let getAll :<|> filterCards :<|> getOne :<|> getSets :<|> getTypes = public
    theUsername <- runIO $ readEnv "dominionAdminUser"
    thePassword <- runIO $ readEnv "dominionAdminPassword" 
    let insert :<|> update :<|> delete = private $ BasicAuthData theUsername thePassword
    let unauthorisedInsert :<|> unauthorisedUpdate :<|> unauthorisedDelete
            = private $ BasicAuthData theUsername "notthepassword"
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8888"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl

    describe "private API for POST/PUT/DELETE" $ do
        let fooCard = CardWithTypesAndLinks (Card
                "foo-card" S.Seaside (Just 0) (Just False) (Just 0) "I do stuff"
                Nothing False Nothing Nothing Nothing Nothing Nothing False
                ) [S.Action] []
        let barCard = CardWithTypesAndLinks (Card
                "bar-card" S.Seaside (Just 0) (Just False) (Just 0) "I do stuff"
                Nothing False Nothing Nothing Nothing Nothing Nothing False
                ) [S.Action] []
        it "POST a new card" $ do
            result <- runClientM (insert copperCard) clientEnv
            getResponseError result `shouldBe` Nothing
        it "but not with incorrect credentials" $ do
            result <- runClientM (unauthorisedInsert copperCard) clientEnv
            getResponseError result `shouldNotBe` Nothing
            getStatus result `shouldBe` Just 403
        it "PUT request works without error" $ do
            -- insert new, "fake" card - the details are unimportant
            runClientM (insert fooCard) clientEnv
            result <- runClientM (update "foo-card" barCard) clientEnv
            getResponseError result `shouldBe` Nothing
        it "but not with incorrect credentials" $ do
            resultUnauth <- runClientM (unauthorisedUpdate "bar-card" fooCard) clientEnv
            getResponseError resultUnauth `shouldNotBe` Nothing
            getStatus resultUnauth `shouldBe` Just 403
        it "after updating the name with PUT, the card can't be found under the old name" $ do
            fooCardResponse <- runClientM (getOne "foo-card") clientEnv
            getError fooCardResponse `shouldNotBe` Nothing
        it "but can under the new" $ do
            barCardResponse <- runClientM (getOne "bar-card") clientEnv
            getError barCardResponse `shouldBe` Nothing
        it "can't delete with wrong credentials" $ do
            resultUnauth <- runClientM (unauthorisedDelete "bar-card") clientEnv
            getResponseError resultUnauth `shouldNotBe` Nothing
            getStatus resultUnauth `shouldBe` Just 403
        it "but can with the correct ones" $ do
            result <- runClientM (delete "bar-card") clientEnv
            getResponseError result `shouldBe` Nothing
        it "resulting in the card not being there" $ do
            barCardResponse <- runClientM (getOne "bar-card") clientEnv
            getError barCardResponse `shouldNotBe` Nothing

    describe "simple GET requests:" $ do
        it "the /cards route should succeed and result in some cards" $ do
            result <- runClientM getAll clientEnv
            getResultList result `shouldNotSatisfy` null
        it "getting a single card which exists should return 1 result" $ do
            result <- runClientM (getOne "copper") clientEnv
            getResultSingle result `shouldNotBe` Nothing
        it "getting a single card with doesn't exist should give an error" $ do
            result <- runClientM (getOne "fake-card") clientEnv
            getError result  `shouldNotBe` Nothing

    describe "testing filters:" $ do
        -- insert the cards we're going to use (in addition to the Copper that's already in
        -- the database). Note that text is irrelevant to the API requests so we leave that
        -- out for simplicity
        it "setting up by inserting more cards" $ do
            let provinceCard = CardWithTypesAndLinks
                    (Card "province" S.Base (Just 8) (Just False) (Just 0) ""
                        Nothing False Nothing Nothing Nothing Nothing Nothing False)
                    [S.Victory] []
            runClientM (insert provinceCard) clientEnv
            let chapelCard = CardWithTypesAndLinks
                    (Card "chapel" S.Base (Just 2) (Just False) (Just 0) ""
                        Nothing True (Just S.Never) (Just S.Never) (Just S.Never)
                        (Just S.Never) (Just S.Never) True)
                    [S.Action] []
            runClientM (insert chapelCard) clientEnv
            let festivalCard = CardWithTypesAndLinks
                    (Card "festival" S.Base (Just 5) (Just False) (Just 0) ""
                        Nothing True (Just S.Always) (Just S.Always) (Just S.Never)
                        (Just S.Never) (Just S.Always) False)
                    [S.Action] []
            runClientM (insert festivalCard) clientEnv
            let adventurerCard = CardWithTypesAndLinks
                    (Card "adventurer" S.BaseFirstEd (Just 6) (Just False) (Just 0) ""
                        Nothing True (Just S.Never) (Just S.Never) (Just S.Always)
                        (Just S.Always) (Just S.Never) False)
                    [S.Action] []
            runClientM (insert adventurerCard) clientEnv
            let vassalCard = CardWithTypesAndLinks
                    (Card "vassal" S.BaseSecondEd (Just 3) (Just False) (Just 0) ""
                        Nothing True (Just S.Sometimes) (Just S.Sometimes) (Just S.Sometimes)
                        (Just S.Sometimes) (Just S.Sometimes) False)
                    [S.Action] []
            runClientM (insert vassalCard) clientEnv
            let pearlDiverCard = CardWithTypesAndLinks
                    (Card "pearl-diver" S.Seaside (Just 2) (Just False) (Just 0) ""
                        Nothing True (Just S.Always) (Just S.Never) (Just S.Always)
                        (Just S.Never) (Just S.Never) False)
                    [S.Action] []
            runClientM (insert pearlDiverCard) clientEnv
            let islandCard = CardWithTypesAndLinks
                    (Card "island" S.Seaside (Just 4) (Just False) (Just 0) ""
                        Nothing True (Just S.Never) (Just S.Never) (Just S.Never)
                        (Just S.Never) (Just S.Never) True)
                    [S.Action, S.Victory] []
            runClientM (insert islandCard) clientEnv
            let ambassadorCard = CardWithTypesAndLinks
                    (Card "ambassador" S.Seaside (Just 3) (Just False) (Just 0) ""
                        Nothing True (Just S.Never) (Just S.Never) (Just S.Never)
                        (Just S.Never) (Just S.Never) True)
                    [S.Action, S.Attack] []
            runClientM (insert ambassadorCard) clientEnv
            let possessionCard = CardWithTypesAndLinks
                    (Card "possession" S.Alchemy (Just 6) (Just True) (Just 0) ""
                        Nothing True (Just S.Never) (Just S.Never) (Just S.Never)
                        (Just S.Never) (Just S.Never) False)
                    [S.Action] []
            runClientM (insert possessionCard) clientEnv
            let potionCard = CardWithTypesAndLinks
                    (Card "potion" S.Alchemy (Just 4) (Just False) (Just 0) ""
                        Nothing False Nothing Nothing Nothing Nothing Nothing False)
                    [S.Treasure] ["possession"]
            runClientM (insert potionCard) clientEnv
            let alchemistCard = CardWithTypesAndLinks
                    (Card "alchemist" S.Alchemy (Just 3) (Just True) (Just 0) ""
                        Nothing True (Just S.Always) (Just S.Never) (Just S.Always)
                        (Just S.Always) (Just S.Never) False)
                    [S.Action] ["potion"]
            runClientM (insert alchemistCard) clientEnv
            let engineerCard = CardWithTypesAndLinks
                    (Card "engineer" S.Empires (Just 0) (Just False) (Just 4) ""
                        Nothing True (Just S.Never) (Just S.Never) (Just S.Never)
                        (Just S.Never) (Just S.Never) False)
                    [S.Action] []
            runClientM (insert engineerCard) clientEnv
            return ()
        it "filter for sets" $ do
            result <- runClientM (filterCards [LenientData $ Right S.BaseFirstEd] Nothing Nothing Nothing
                        Nothing False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 5
            filteredCardNames `shouldContain` ["copper"]
            filteredCardNames `shouldContain` ["province"]
            filteredCardNames `shouldContain` ["chapel"]
            filteredCardNames `shouldContain` ["festival"]
            filteredCardNames `shouldContain` ["adventurer"]
        it "with both first and second editions" $ do
            result <- runClientM (filterCards [LenientData $ Right S.Base] Nothing Nothing Nothing
                        Nothing False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 4
            filteredCardNames `shouldContain` ["copper"]
            filteredCardNames `shouldContain` ["province"]
            filteredCardNames `shouldContain` ["chapel"]
            filteredCardNames `shouldContain` ["festival"]
        it "with more than one set" $ do
            result <- runClientM (filterCards [LenientData $ Right S.Seaside,
                        LenientData $ Right S.Alchemy] Nothing Nothing Nothing
                        Nothing False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 6
            filteredCardNames `shouldContain` ["pearl-diver"]
            filteredCardNames `shouldContain` ["island"]
            filteredCardNames `shouldContain` ["ambassador"]
            filteredCardNames `shouldContain` ["potion"]
            filteredCardNames `shouldContain` ["possession"]
            filteredCardNames `shouldContain` ["alchemist"]
        it "filter for minimum coin cost" $ do
            result <- runClientM (filterCards [] (Just 0) Nothing Nothing
                        Nothing False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 13
            -- no need to test individual names as there are only 13 cards in the test set
        it "and again with a higher minimum" $ do
            result <- runClientM (filterCards [] (Just 6) Nothing Nothing
                        Nothing False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 3
            filteredCardNames `shouldContain` ["province"]
            filteredCardNames `shouldContain` ["adventurer"]
            filteredCardNames `shouldContain` ["possession"]
        it "filter for maximum coin cost" $ do
            result <- runClientM (filterCards [] Nothing (Just 8) Nothing
                        Nothing False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 13
        it "and again with a lower maximum" $ do
            result <- runClientM (filterCards [] Nothing (Just 0) Nothing
                        Nothing False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 2
            filteredCardNames `shouldContain` ["copper"]
            filteredCardNames `shouldContain` ["engineer"]
        it "filter for needing potion" $ do
            result <- runClientM (filterCards [] Nothing Nothing (Just True)
                        Nothing False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 2
            filteredCardNames `shouldContain` ["possession"]
            filteredCardNames `shouldContain` ["alchemist"]
        it "and for not needing it" $ do
            result <- runClientM (filterCards [] Nothing Nothing (Just False)
                        Nothing False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 11
            filteredCardNames `shouldContain` ["copper"]
            filteredCardNames `shouldContain` ["province"]
            filteredCardNames `shouldContain` ["chapel"]
            filteredCardNames `shouldContain` ["festival"]
            filteredCardNames `shouldContain` ["adventurer"]
            filteredCardNames `shouldContain` ["vassal"]
            filteredCardNames `shouldContain` ["pearl-diver"]
            filteredCardNames `shouldContain` ["island"]
            filteredCardNames `shouldContain` ["ambassador"]
            filteredCardNames `shouldContain` ["potion"]
            filteredCardNames `shouldContain` ["engineer"]
        it "filter for needing debt" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing (Just True)
                        False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 1
            filteredCardNames `shouldContain` ["engineer"]
        it "and for not needing it" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing (Just False)
                        False Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 12
            filteredCardNames `shouldContain` ["copper"]
            filteredCardNames `shouldContain` ["province"]
            filteredCardNames `shouldContain` ["chapel"]
            filteredCardNames `shouldContain` ["festival"]
            filteredCardNames `shouldContain` ["adventurer"]
            filteredCardNames `shouldContain` ["vassal"]
            filteredCardNames `shouldContain` ["pearl-diver"]
            filteredCardNames `shouldContain` ["island"]
            filteredCardNames `shouldContain` ["ambassador"]
            filteredCardNames `shouldContain` ["potion"]
            filteredCardNames `shouldContain` ["possession"]
            filteredCardNames `shouldContain` ["alchemist"]
        it "filter for just Kingdom cards" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing
                        True Nothing Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 10
            filteredCardNames `shouldContain` ["chapel"]
            filteredCardNames `shouldContain` ["festival"]
            filteredCardNames `shouldContain` ["adventurer"]
            filteredCardNames `shouldContain` ["vassal"]
            filteredCardNames `shouldContain` ["pearl-diver"]
            filteredCardNames `shouldContain` ["island"]
            filteredCardNames `shouldContain` ["ambassador"]
            filteredCardNames `shouldContain` ["possession"]
            filteredCardNames `shouldContain` ["alchemist"]
            filteredCardNames `shouldContain` ["engineer"]
        it "filter for cards which are sometimes non-terminal" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                        (Just S.CanSometimes) Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 4
            filteredCardNames `shouldContain` ["festival"]
            filteredCardNames `shouldContain` ["vassal"]
            filteredCardNames `shouldContain` ["pearl-diver"]
            filteredCardNames `shouldContain` ["alchemist"]
        it "and for cards which are always non-terminal" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                        (Just S.CanAlways) Nothing Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 3
            filteredCardNames `shouldContain` ["festival"]
            filteredCardNames `shouldContain` ["pearl-diver"]
            filteredCardNames `shouldContain` ["alchemist"]
        it "filter for cards which are sometimes villages" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                        Nothing (Just S.CanSometimes) Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 2
            filteredCardNames `shouldContain` ["festival"]
            filteredCardNames `shouldContain` ["vassal"]
        it "and for cards which are always villages" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                         Nothing (Just S.CanAlways) Nothing Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 1
            filteredCardNames `shouldContain` ["festival"]
        it "filter for cards which sometimes don't reduce your hand-size" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                        Nothing Nothing (Just S.CanSometimes) Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 4
            filteredCardNames `shouldContain` ["adventurer"]
            filteredCardNames `shouldContain` ["vassal"]
            filteredCardNames `shouldContain` ["pearl-diver"]
            filteredCardNames `shouldContain` ["alchemist"]
        it "and for cards which never reduce hand-size" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                        Nothing Nothing (Just S.CanAlways) Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 3
            filteredCardNames `shouldContain` ["adventurer"]
            filteredCardNames `shouldContain` ["pearl-diver"]
            filteredCardNames `shouldContain` ["alchemist"]
        it "filter for cards which can increase your hand-size" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                        Nothing Nothing Nothing (Just S.CanSometimes) False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 3
            filteredCardNames `shouldContain` ["adventurer"]
            filteredCardNames `shouldContain` ["vassal"]
            filteredCardNames `shouldContain` ["alchemist"]
        it "and for cards which always draw" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                        Nothing Nothing Nothing (Just S.CanAlways) False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 2
            filteredCardNames `shouldContain` ["adventurer"]
            filteredCardNames `shouldContain` ["alchemist"]
        it "filter for cards which (pseudo-)trash)" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                        Nothing Nothing Nothing Nothing True Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 3
            filteredCardNames `shouldContain` ["chapel"]
            filteredCardNames `shouldContain` ["island"]
            filteredCardNames `shouldContain` ["ambassador"]
        it "filter for cards which can give an extra buy" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False Nothing
                        Nothing Nothing Nothing False (Just S.CanSometimes) [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 2
            filteredCardNames `shouldContain` ["festival"]
            filteredCardNames `shouldContain` ["vassal"]
        it "and for cards which always give an extra buy" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False Nothing
                        Nothing Nothing Nothing False (Just S.CanAlways) [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 1
            filteredCardNames `shouldContain` ["festival"]
        it "filter by type" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False Nothing
                        Nothing Nothing Nothing False Nothing
                        [LenientData $ Right S.Treasure] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 2
            filteredCardNames `shouldContain` ["copper"]
            filteredCardNames `shouldContain` ["potion"]
        it "with a different type" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False Nothing
                        Nothing Nothing Nothing False Nothing
                        [LenientData $ Right S.Victory] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 2
            filteredCardNames `shouldContain` ["province"]
            filteredCardNames `shouldContain` ["island"]
        it "with two types at once" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False Nothing
                        Nothing Nothing Nothing False Nothing
                        [LenientData $ Right S.Attack, LenientData $ Right S.Victory] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 3
            filteredCardNames `shouldContain` ["province"]
            filteredCardNames `shouldContain` ["island"]
            filteredCardNames `shouldContain` ["ambassador"]
        it "filter by linked cards" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False Nothing
                        Nothing Nothing Nothing False Nothing [] ["potion"]) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 2
            filteredCardNames `shouldContain` ["possession"]
            filteredCardNames `shouldContain` ["alchemist"]
        it "combinations of different filters: cards which always draw and are sometimes nonterminal" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                        (Just S.CanSometimes) Nothing Nothing (Just S.CanAlways)
                        False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 1
            filteredCardNames `shouldContain` ["alchemist"]
        it "(pseudo-)trashers with type Attack" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False
                        Nothing Nothing Nothing Nothing True Nothing
                        [LenientData $ Right S.Attack] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 1
            filteredCardNames `shouldContain` ["ambassador"]
        it "kingdom Victory cards" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing True
                        Nothing Nothing Nothing Nothing False Nothing
                        [LenientData $ Right S.Victory] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 1
            filteredCardNames `shouldContain` ["island"]
        it "cards from one of two sets which sometimes don't reduce handsize" $ do
            result <- runClientM (filterCards
                        [LenientData $ Right S.BaseFirstEd, LenientData $ Right S.Seaside]
                        Nothing Nothing Nothing Nothing False Nothing Nothing
                        (Just S.CanSometimes) Nothing False Nothing [] []) clientEnv
            let filteredCardNames = map (cardName . underlyingCard) $ getResultList result
            length filteredCardNames `shouldBe` 2
            filteredCardNames `shouldContain` ["adventurer"]
            filteredCardNames `shouldContain` ["pearl-diver"]

    describe "miscellaneous other tests" $ do
        it "no repeats in lists of types and linked cards" $ do
            result <- runClientM getAll clientEnv
            let noRepeat xs = length xs == length (nub xs)
            let types (CardWithTypesAndLinks _ ts _) = ts
            let links (CardWithTypesAndLinks _ _ ls) = ls
            let allTypes = map types $ getResultList result
            let allLinks = map links $ getResultList result
            allTypes `shouldSatisfy` all noRepeat
            allLinks `shouldSatisfy` all noRepeat
        it "filtering by link includes all links in the results" $ do
            result <- runClientM (filterCards [] Nothing Nothing Nothing Nothing False Nothing
                        Nothing Nothing Nothing False Nothing [] ["alchemist"]) clientEnv
            let resultList = getResultList result
            length resultList `shouldBe` 1
            let [CardWithTypesAndLinks _ _ links] = resultList
            length links `shouldBe` 2
            links `shouldContain` ["alchemist"]
            links `shouldContain` ["possession"]
        it "the sets endpoint works as intended" $ do
            result <- runClientM getSets clientEnv
            getResultList result `shouldNotSatisfy` null
        it "and so does the types endpoint" $ do
            result <- runClientM getTypes clientEnv
            getResultList result `shouldNotSatisfy` null

    describe "tidying up" $ do
        it "remove all cards to leave database empty" $ do
            let allCards = ["copper", "province", "chapel", "festival", "adventurer",
                    "vassal", "pearl-diver", "island", "ambassador", "possession",
                    "potion", "alchemist", "engineer"];
            mapM_ (flip runClientM clientEnv . delete) allCards


    where getResultSingle = either (const Nothing) S.result
          getResultList = either (const []) (fromMaybe [] . S.result)
          getError = either (const Nothing) S.error
          getResponseError = either Just (const Nothing)
          copperCard = CardWithTypesAndLinks
            (Card "copper" S.Base (Just 0) (Just False) (Just 0) "1 Coin"
                Nothing False Nothing Nothing Nothing Nothing Nothing False)
                [S.Treasure] []
          getStatus maybeErr = do
            clientErr <- getResponseError maybeErr
            case clientErr of
                FailureResponse _ (Response (Status code _) _ _ _) -> Just code
                _ -> Nothing
          underlyingCard (CardWithTypesAndLinks c _ _) = c


main :: IO ()
main = hspec spec
