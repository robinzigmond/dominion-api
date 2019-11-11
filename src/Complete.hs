{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Complete where

import Network.Wai
import Servant

import Auth (PrivateAPI, authContext)
import qualified Auth as Priv
import Docs (PublicAPIWithDocs)
import qualified Docs as Pub


type DominionAPI = PrivateAPI :<|> PublicAPIWithDocs


server :: Server DominionAPI
server = Priv.server :<|> Pub.server


dominionAPI :: Proxy DominionAPI
dominionAPI = Proxy


api :: Application
api = serveWithContext dominionAPI authContext server
