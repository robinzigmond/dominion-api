-- testing, rough plan:

-- use Servant Client to send requests, to test "business logic" (example database? test on real db?)

-- hspec-wai to test more general requests (eg to docs endpoint, or non-existent ones)

-- servant-quickcheck - test some of the "best practices", predicates at
-- https://github.com/haskell-servant/servant-quickcheck/blob/master/src/Servant/QuickCheck/Internal/Predicates.hs
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import Test.Hspec

import Complete (api, DominionAPI)
import qualified SubsidiaryTypes as S


withApi :: IO () -> IO ()
withApi action =
    -- we can spin up a server in another thread and kill that thread when done
    -- in an exception-safe way
    bracket (liftIO $ forkIO $ run 8888 api) killThread (const action)


spec :: Spec
spec = apiTestRequestsSpec -- >> quickQueckSpec


apiTestRequestsSpec :: Spec
apiTestRequestsSpec = around_ withApi $ do
    let private :<|> public :<|> docs = client (Proxy :: Proxy DominionAPI)
    let getAll :<|> getOne :<|> filter :<|> getSets :<|> getTypes = public
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8888"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl

    describe "general GET requests" $ do
        it "- the /cards route should succeed and result in some cards" $ do
            result <- runClientM getAll clientEnv
            either (const []) (fromMaybe [] . S.result) result
                `shouldNotSatisfy` null


quickQueckSpec :: Spec
quickQueckSpec = error "not implemented yet"


main :: IO ()
main = hspec spec
