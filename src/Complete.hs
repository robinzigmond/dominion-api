{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Complete where

import Network.Wai
import Network.Wai.Middleware.Cors
import Servant

import Auth (PrivateAPI, authContext)
import qualified Auth as Priv
import Database (RunDB)
import Docs (PublicAPIWithDocs)
import qualified Docs as Pub


type DominionAPI = PrivateAPI :<|> PublicAPIWithDocs


server :: (forall a. RunDB a) -> Server DominionAPI
server runDB = Priv.server runDB :<|> Pub.server runDB


dominionAPI :: Proxy DominionAPI
dominionAPI = Proxy


api :: (forall a. RunDB a) -> Application
api runDB = simpleCors $ serveWithContext dominionAPI authContext $ server runDB
