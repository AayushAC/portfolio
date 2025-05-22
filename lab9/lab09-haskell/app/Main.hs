{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeOperators           #-}

module Main where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON, eitherDecode)
import           Data.ByteString.Lazy.Char8  (ByteString)
import qualified Data.ByteString.Lazy.Char8  as BL
import qualified Data.ByteString.Char8       as BS
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.Generics                (Generic)
import           Network.HTTP.Client         (newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Simple         (httpLBS, getResponseBody, setRequestHeader, parseRequest, setRequestManager)
import           Network.Wai.Handler.Warp    (run)
import           Servant
import           System.Environment          (lookupEnv)
import           Configuration.Dotenv        (loadFile, defaultConfig)
import           Data.Maybe                  (fromMaybe)

-- | Response for GET /greet/:name
data GreetResponse = GreetResponse
  { greet :: Text
  , name  :: Text
  } deriving (Generic, Show)
instance ToJSON GreetResponse

-- | Request body for POST /greetme
data GreetMeInput = GreetMeInput
  { input :: Text
  , name  :: Text
  } deriving (Generic, Show)
instance FromJSON GreetMeInput
instance ToJSON GreetMeInput

-- | Response for POST /greetme
data MsgResponse = MsgResponse
  { msg :: Text
  } deriving (Generic, Show)
instance ToJSON MsgResponse

-- | One announcement pulled from the GitLab API
data Announcement = Announcement
  { title      :: Text
  , description:: Text
  , created_at :: Text
  } deriving (Generic, Show)
instance FromJSON Announcement
instance ToJSON Announcement

-- | API definition
type API =
       "hello" :> Get '[PlainText] Text
  :<|> "greet" :> Capture "name" Text :> Get '[JSON] GreetResponse
  :<|> "greetme" :> ReqBody '[JSON] GreetMeInput :> Post '[JSON] MsgResponse
  :<|> "announcements.json" :> Get '[JSON] [Announcement]
  :<|> "announcements.html" :> Get '[PlainText] Text

api :: Proxy API
api = Proxy

-- | Server implementation
server :: Server API
server =
       helloHandler
  :<|> greetHandler
  :<|> greetMeHandler
  :<|> announcementsJSONHandler
  :<|> announcementsHTMLHandler

  where
    helloHandler :: Handler Text
    helloHandler = return "Hello, World!"

    greetHandler :: Text -> Handler GreetResponse
    greetHandler nm = return $ GreetResponse "Hello" nm

    greetMeHandler :: GreetMeInput -> Handler MsgResponse
    greetMeHandler (GreetMeInput input name) =
      return $ MsgResponse (input <> " " <> name)

    announcementsJSONHandler :: Handler [Announcement]
    announcementsJSONHandler = do
      manager <- liftIO $ newManager tlsManagerSettings
      maybeToken <- liftIO $ lookupEnv "GITLAB_ACCESS_TOKEN"
      case maybeToken of
        Nothing -> throwError err500 { errBody = "Missing GITLAB_ACCESS_TOKEN in environment" }
        Just tk -> do
          let token = BS.pack tk
          request <- liftIO $ parseRequest "https://git.gvk.idi.ntnu.no/api/v4/projects/5881/issues?labels=Announcement&state=opened"
          let authenticatedRequest = setRequestHeader "PRIVATE-TOKEN" [token] $
                                     setRequestManager manager request
          response <- liftIO $ httpLBS authenticatedRequest
          let body :: ByteString
              body = getResponseBody response
          case eitherDecode body of
            Right anns -> return anns
            Left err   -> do
              liftIO $ putStrLn $ "JSON parse error: " ++ err ++ "\nBody: " ++ BL.unpack body
              throwError err500 { errBody = "JSON parse error: " <> BL.pack err }

    announcementsHTMLHandler :: Handler Text
    announcementsHTMLHandler = do
      anns <- announcementsJSONHandler
      let formatAnn a =
            "- " <> title a
              <> ": " <> description a
              <> " (" <> created_at a <> ")"
      return $ T.unlines (map formatAnn anns)

-- | Main entry point
main :: IO ()
main = do
  -- Load .env file
  _ <- loadFile defaultConfig
  putStrLn "Starting server on port 8080..."
  run 8080 (serve api server)
