module Servant.WebSockets.Examples.MyExample where

import Control.Monad
import Data.Proxy

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isPunctuation, isSpace)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API ((:>), Header', Required, Strict)
import Servant.API.Generic ((:-), ToServantApi, genericApi)
import Servant.Client
  ( BaseUrl(BaseUrl, baseUrlHost, baseUrlPath, baseUrlPort, baseUrlScheme), Scheme(Http)
  , mkClientEnv, runClientM
  )
import Servant.Client.Generic (AsClientT, genericClientHoist)
import Servant.Server (Application, Handler)
import Servant.Server.Generic (AsServer, genericServe)
import Servant.WebSockets.API (WebSocketApp)
import Servant.WebSockets.Client ()
import Servant.WebSockets.Server ()

import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.WebSockets as WS

type MyAPI = ToServantApi MyRoutes

myAPI :: Proxy MyAPI
myAPI = genericApi (Proxy @MyRoutes)

data MyRoutes routes = MyRoutes
  { chat :: routes :-
      Header' '[Required, Strict] "Authorization" User
        :> "chat"
        :> WebSocketApp
  } deriving stock (Generic)

type MyServer = MyRoutes AsServer

mkMyServer :: MVar MyServerState -> MyServer
mkMyServer state = MyRoutes { chat }
  where
  chat :: User -> WS.PendingConnection -> Handler ()
  chat user pendingConn = liftIO $ do
    s <- readMVar state
    if user == "Bill" then
      WS.rejectRequestWith pendingConn
        WS.RejectRequest
          { WS.rejectCode = 403
          , WS.rejectMessage = "Forbidden"
          , WS.rejectHeaders = []
          , WS.rejectBody = "No Bills allowed!"
          }
    else if any ($ user) [Text.null, Text.any isPunctuation, Text.any isSpace] then
      WS.rejectRequest pendingConn "Name cannot contain punctuation or whitespace, and cannot be empty"
    else if any (user ==) (map fst s) then
      WS.rejectRequest pendingConn "User already exists"
    else
      start =<< WS.acceptRequest pendingConn
    where
    start conn = do
      WS.withPingThread conn 30 (pure ()) $ do -- TODO use bracket here
        flip finally disconnect $ do
          s <- readMVar state
          WS.sendTextData conn $ case s of
            [] -> "Welcome!"
            _  -> "Welcome! Users: " <> Text.intercalate ", " (map fst s)
          broadcast $ user <> " joined"
          modifyMVar_ state $ pure . ((user, conn) :)
          talk conn

    disconnect = do
      modifyMVar_ state $ pure . filter ((user /=) . fst)
      broadcast (user <> " disconnected")

    broadcast message = do
      s <- readMVar state
      forM_ s $ \(_, conn) -> WS.sendTextData conn message

    talk conn = forever $ do
      message <- WS.receiveData conn
      broadcast $ user <> ": " <> message

type User = Text
type MyServerState = [(User, WS.Connection)]

type MyClient = MyRoutes (AsClientT IO)

mkMyClient :: HTTPClient.Manager -> Int -> MyClient
mkMyClient httpManager port =
  genericClientHoist $ \x ->
    runClientM x clientEnv >>= either throwIO pure
  where
  clientEnv =
    mkClientEnv httpManager $
      BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost = "127.0.0.1"
        , baseUrlPort = port
        , baseUrlPath = ""
        }

newMyWaiApp :: IO Application
newMyWaiApp = do
  state <- newMVar []
  pure $ mkMyWaiApp state

mkMyWaiApp :: MVar MyServerState -> Application
mkMyWaiApp state = genericServe $ mkMyServer state
