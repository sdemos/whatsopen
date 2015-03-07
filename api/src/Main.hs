{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Control.Applicative
import Control.Monad.Trans
import Data.Proxy
import Data.Convertible
import System.FilePath.Posix
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as B

-- Time
import Data.Time.LocalTime
import Data.Time.Clock

-- PostgreSQL
import Database.HDBC hiding (run)
import Database.HDBC.PostgreSQL

-- Servant/web server stuff
import Network.Wai.Handler.Warp (run)
import Network.Wai
import Network.HTTP.Types (status200)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (hamletFile)
import Servant
import Servant.JQuery

-- project files
import Config
import DataDef

www :: FilePath
www = "../frontend"

main :: IO ()
main = do
    writeJS (www </> "api.js") [openJS]
    run 8080 (serve whatsOpenAPI server)

writeJS :: FilePath -> [AjaxReq] -> IO ()
writeJS fp funs = writeFile fp $ concatMap generateJS funs

openJS :: AjaxReq
openJS :<|> _ = jquery whatsOpenAPI

whatsOpenAPI :: Proxy WhatsOpenAPI
whatsOpenAPI = Proxy

type WhatsOpenAPI = "open" :> Get [Open]
--               :<|> "open" :> Capture "timestamp" LocalTime :> Get [Open]
               :<|> "static" :> Raw
               :<|> Raw

server :: Server WhatsOpenAPI
server = liftIO whatsOpen
--    :<|> liftIO . openAt
    :<|> serveDirectory www
    :<|> whatsOpenApp

htmlApp :: Html -> Application
htmlApp = stringApp . renderHtml

stringApp :: B.ByteString -> Application
stringApp s _ respond = respond $ responseLBS status200 [] s

--whatsOpenHtml :: Application
--whatsOpenHtml = htmlApp $ $(hamletFile "whatsopen.hamlet") woUrlRender

whatsOpenApp :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
whatsOpenApp _ respond = do
    openStores <- whatsOpen
    respond $ responseLBS status200 []  $ renderHtml $ $(hamletFile "whatsopen.hamlet") woUrlRender

data WORoute = Stylesheet | BootstrapCss | BootstrapJs | CSH | SDemos | Github

woUrlRender :: WORoute -> [(Text, Text)] -> Text
woUrlRender Stylesheet _ = "/static/dev/whatsopen.css"
woUrlRender BootstrapCss _ = "/static/bootstrap-csh/node_modules/bootstrap/dist/css/bootstrap.css"
woUrlRender BootstrapJs _ = "/static/bootstrap-csh/node_modules/bootstrap/dist/js/bootstrap.js"
woUrlRender CSH _ = "http://csh.rit.edu/"
woUrlRender SDemos _ = "http://sdemos.com/"
woUrlRender Github _ = "https://github.com/stphndemos/whatsopen"

query :: Convertible [SqlValue] b => String -> [SqlValue] -> IO [b]
query q p = do
    conn <- connection
    map convert <$> quickQuery conn q p

query_ :: Convertible [SqlValue] b => String -> IO [b]
query_ = flip query []

connection :: IO Connection
connection = connectPostgreSQL conninfo

whatsOpen :: IO [Open]
whatsOpen = getCurrentLocalTime >>= openAt

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

openAt :: LocalTime -> IO [Open]
openAt time = map (consOpen (localTimeOfDay time)) <$> getOpenStores time

stores :: IO [Store]
stores = query_ "select * from whatsopen.stores"

getHours :: LocalTime -> Store -> IO Day
getHours t s = Day s <$> map consHours <$> query "select whatsopen.get_hours(?, ?)" [toSql t, toSql (storeId s)]

consOpen :: TimeOfDay -> Day -> Open
consOpen time day = Open { store = getDayStore day
                         , openFor = timeOfDayToTime closeTime - timeOfDayToTime time
                         , openTill = closeTime
                         }
    where closeTimes = map getCloseTime (getDayHours day)
          --closeTime = maybe time id (find ((>time) . getCloseTime) dayHours)
          closeTime = maybe time id (find (>time) closeTimes)

consHours :: (TimeOfDay, TimeOfDay) -> Hours
consHours = uncurry Hours

getOpenStores :: LocalTime -> IO [Day]
getOpenStores time = filter (openDuring (localTimeOfDay time)) <$> (stores >>= mapM (getHours time))
    where openDuring t (Day _ hs) = or (map (openDuring' t) hs)
          openDuring' t (Hours open close) = open < t && t < close
