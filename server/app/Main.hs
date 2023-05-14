module Main where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe, fromJust)

-- Local dev
import Network.Wai.Middleware.Cors



type UserApi =
  "user" :> Get '[JSON] [User] :<|>
  "user" :> Capture "name" String :> Get '[JSON] User

userApi :: Proxy UserApi
userApi = Proxy

main :: IO ()
main = do
  port <- fromMaybe "8080" <$> lookupEnv "PORT"
  putStrLn $ "running on port " ++ port
  run (read port) app

app :: Application
app = simpleCors (serve userApi server)

server :: Server UserApi
server =
  getUsers :<|>
  getUserByName 

getUsers :: Handler [User]
getUsers = return [User "Isaac" 20, User "Ashley" 21, User "Gob" 8]

getUserByName :: String -> Handler User
getUserByName s = case s of
  "Isaac" -> return $ User "Isaac" 20
  "Ashley" -> return $ User "Ashley" 21
  _ -> throwError err404

data User = User { name :: String, age :: Int } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
