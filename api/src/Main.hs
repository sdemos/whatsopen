{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
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
import Control.Monad.Trans (liftIO)

-- Time
import Data.Time.LocalTime
import Data.Time.Clock

-- Servant/web server stuff
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Blaze

-- project files
import DataDef

main :: IO ()
main = do 
    port <- getEnv "PORT"
    run (read port) (serve (Proxy :: Proxy WhatsOpenAPI) server)

type WhatsOpenAPI = Get '[JSON, HTML] [Open]
               :<|> "open" :> Capture "timestamp" LocalTime :> Get '[JSON, HTML] [Open]
--               :<|> "hours" :> Capture "store" Int32 :> Get '[JSON, HTML] [Day]
--               :<|> "hours" :> Capture "store" Int32 :> Capture "timestamp" Localtime :> Get '[JSON, HTML] [Day]
               :<|> "stores" :> Get '[JSON, HTML] [Store]
--               :<|> "stores" :> Capture "store" Int32 :> Get '[JSON, HTML] Store
               :<|> "static" :> Raw

server :: Server WhatsOpenAPI
server = liftIO whatsOpen
    :<|> liftIO . openAt
--    :<|> notImplemented
--    :<|> notImplemented
    :<|> liftIO stores
--    :<|> notImplemented
    :<|> serveDirectory "../frontend"

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
