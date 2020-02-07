{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import System.Directory (removeFile)
import System.IO
import Test.Hspec

import Complete (api, DominionAPI)
import Database (runTestDB, Card(..), readEnv, testDBName)
import Instances (CardWithTypesAndLinks(..))
import qualified SubsidiaryTypes as S


withApi :: IO () -> IO ()
withApi action =
    -- we can spin up a server in another thread and kill that thread when done
    -- in an exception-safe way
    bracket (liftIO . forkIO . run 8888 $ api runTestDB) killThread (const action)


spec :: Spec
spec = apiTestRequestsSpec


apiTestRequestsSpec :: Spec
apiTestRequestsSpec = around_ withApi $ do
    let private :<|> public :<|> docs = client (Proxy :: Proxy DominionAPI)
    let getAll :<|> filter :<|> getOne :<|> getSets :<|> getTypes = public
    theUsername <- runIO $ readEnv "dominionAdminUser"
    thePassword <- runIO $ readEnv "dominionAdminPassword" 
    let insert :<|> update :<|> delete = private $ BasicAuthData theUsername thePassword
    let unauthorisedInsert :<|> unauthorisedUpdate :<|> unauthorisedDelete
            = private $ BasicAuthData theUsername "notthepassword"
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8888"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl
    runIO $ openFile (unpack testDBName) WriteMode

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
            getResponseError result `shouldNotBe` Nothing -- check for 403/401 precisely?
        it "PUT request works without error" $ do
            -- insert new, "fake" card - the details are unimportant
            runClientM (insert fooCard) clientEnv
            result <- runClientM (update "foo-card" barCard) clientEnv
            getResponseError result `shouldBe` Nothing
        it "but not with incorrect credentials" $ do
            resultUnauth <- runClientM (unauthorisedUpdate "bar-card" fooCard) clientEnv
            getResponseError resultUnauth `shouldNotBe` Nothing
        it "after updating the name with PUT, the card can't be found under the old name" $ do
            fooCardResponse <- runClientM (getOne "foo-card") clientEnv
            getError fooCardResponse `shouldNotBe` Nothing
        it "but can under the new" $ do
            barCardResponse <- runClientM (getOne "bar-card") clientEnv
            getError barCardResponse `shouldBe` Nothing
        it "can't delete with wrong credentials" $ do
            resultUnauth <- runClientM (unauthorisedDelete "bar-card") clientEnv
            getResponseError resultUnauth `shouldNotBe` Nothing
        it "but can with the correct ones" $ do
            result <- runClientM (delete "bar-card") clientEnv
            getResponseError result `shouldBe` Nothing
        it "resulting in the card not being there" $ do
            barCardResponse <- runClientM (getOne "bar-card") clientEnv
            getError barCardResponse `shouldNotBe` Nothing

    describe "general GET requests:" $ do
        it "the /cards route should succeed and result in some cards" $ do
            result <- runClientM getAll clientEnv
            getResultList result `shouldNotSatisfy` null
        it "getting a single card which exists should return 1 result" $ do
            result <- runClientM (getOne "copper") clientEnv
            getResultSingle result `shouldNotBe` Nothing
        it "getting a single card with doesn't exist should give an error" $ do
            result <- runClientM (getOne "fake-card") clientEnv
            getError result  `shouldNotBe` Nothing
        -- tests for filters, singly and in combination
        -- tests for linked cards being all present and correct, in each type of request
        -- test for no repeats in arrays of types/links

    -- make sure test database is deleted at the end of the run
    runIO . removeFile $ unpack testDBName

        where getResultSingle = either (const Nothing) S.result
              getResultList = either (const []) (fromMaybe [] . S.result)
              getError = either (const Nothing) S.error
              getResponseError = either Just (const Nothing)
              copperCard = CardWithTypesAndLinks
                (Card "copper" S.Base (Just 0) (Just False) (Just 0) "1 Coin"
                    Nothing False Nothing Nothing Nothing Nothing Nothing False)
                [S.Treasure] []


main :: IO ()
main = hspec spec
