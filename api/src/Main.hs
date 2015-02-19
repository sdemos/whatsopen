{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Control.Applicative
import Control.Monad.Trans
import Data.Proxy
import Data.Convertible

-- Time
import Data.Time.LocalTime
import Data.Time.Clock

-- PostgreSQL
import Database.HDBC hiding (run)
import Database.HDBC.PostgreSQL

-- Servant/web server stuff
import Network.Wai.Handler.Warp (run)
import Servant

-- project files
import Config
import DataDef

main :: IO ()
main = run 8080 (serve whatsOpenAPI server)

whatsOpenAPI :: Proxy WhatsOpenAPI
whatsOpenAPI = Proxy

server :: Server WhatsOpenAPI
--server = liftIO whatsOpen :<|> liftIO . openAt
server = liftIO whatsOpen

type WhatsOpenAPI = "open" :> Get [Open]
--               :<|> "open" :> Capture "timestamp" LocalTime :> Get [Open]

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
