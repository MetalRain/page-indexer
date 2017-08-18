module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import qualified PageInfo as PI
import qualified Page as P

type API = "pages" :> Get '[JSON] [P.Page]
  :<|> "pages" :> ReqBody '[JSON] PI.PageInfo :> Post '[JSON] P.Page 

startApp :: IO ()
startApp = run 8000 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return pages
  :<|> postPage

pages :: [P.Page]
pages = [ P.Page "https://google.com" ["search", "google"]
        , P.Page "https://youtube.com" ["video", "media"]
        ]

postPage :: PI.PageInfo -> Handler P.Page
postPage info = return (P.Page (PI.url info) [])
