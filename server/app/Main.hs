module Main where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as Map

-- Local dev
import Network.Wai.Middleware.Cors


-- type UserApi =
--   "user" :> Get '[JSON] [User] :<|>
--   "user" :> Capture "name" String :> Get '[JSON] User

data Room = Room String deriving (Eq, Show, Generic)

instance ToJSON Room
instance FromJSON Room

type Api =
  Capture "username" String :> Capture "code" String :> Post '[JSON] Room


userApi :: Proxy Api
userApi = Proxy

main :: IO ()
main = do
  let port = "8080"
  putStrLn $ "running on port " ++ port
  run (read port) app

app :: Application
app = simpleCors (serve userApi server)

server :: Server Api
server =
  joinRoom

joinRoom :: String -> String -> Handler Room
joinRoom username roomcode = 
  maybe (throwError err404) pure (Map.lookup roomcode rooms)


rooms :: Map.Map String Room
rooms = Map.fromList [("ASDF", Room "hi")]

