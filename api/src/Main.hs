{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.List
import Data.Maybe (fromMaybe)
import System.Environment
import Data.Proxy
import Data.Text (Text)
import Control.Monad.Trans (liftIO)

-- Time
import Data.Time.LocalTime
import Data.Time.Clock

-- Servant/web server stuff
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (hamletFile)
import qualified Network.HTTP.Media as M
import Servant

-- project files
import Database
import DataDef

main :: IO ()
main = do 
    port <- getEnv "PORT"
    run (read port) (serve (Proxy :: Proxy WhatsOpenAPI) server)

-- lets try adding an HTML content type - text/html
-- it is going to be using the Text.Blaze.Html Html datatype
instance Accept Html where
    contentType _ = "text" M.// "html"

instance MimeRender Html [Open] where
    mimeRender _ openStores = renderHtml $ $(hamletFile "templates/whatsopen.hamlet") woUrlRender

type WhatsOpenAPI = Get '[JSON, Html] [Open]
               :<|> Capture "timestamp" LocalTime :> Get '[JSON, Html] [Open]
               :<|> "static" :> Raw

server :: Server WhatsOpenAPI
server = liftIO whatsOpen
    :<|> liftIO . openAt
    :<|> serveDirectory "../frontend"

data WORoute = Stylesheet | BootstrapCss | BootstrapJs | CSH | SDemos | Github

woUrlRender :: WORoute -> [(Text, Text)] -> Text
woUrlRender Stylesheet _   = "/static/dev/whatsopen.css"
woUrlRender BootstrapCss _ = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/css/bootstrap.min.css"
woUrlRender BootstrapJs _  = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/js/bootstrap.min.js"
woUrlRender CSH _          = "http://csh.rit.edu/"
woUrlRender SDemos _       = "http://sdemos.com/"
woUrlRender Github _       = "https://github.com/sdemos/whatsopen"

whatsOpen :: IO [Open]
whatsOpen = getCurrentLocalTime >>= openAt

openAt :: LocalTime -> IO [Open]
openAt time = map (consOpen (localTimeOfDay time)) <$> getOpenStores time

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

getOpenStores :: LocalTime -> IO [Day]
getOpenStores time = filter (openDuring (localTimeOfDay time)) <$> (stores >>= mapM (storeHours time))
    where openDuring t (Day _ hs) = any (openDuring' t) hs
          openDuring' t (Hours open close) = open < t && t < close

consOpen :: TimeOfDay -> Day -> Open
consOpen time day = Open { store    = getDayStore day
                         , openFor  = timeOfDayToTime closeTime - timeOfDayToTime time
                         , openTill = closeTime
                         }
    where closeTimes = map getCloseTime (getDayHours day)
          closeTime = fromMaybe time (find (>time) closeTimes)
