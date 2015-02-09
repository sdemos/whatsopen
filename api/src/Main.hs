{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import           Data.Time.LocalTime
import           Data.Time.Clock.POSIX

import           Database.HDBC
import           Database.HDBC.PostgreSQL    (connectPostgreSQL)

import qualified Data.ByteString as B hiding (map)
import           Data.Maybe                  (fromMaybe)
import           Control.Monad.Trans         (liftIO)

import           Config                      (conninfo)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "WhatsOpen@RIT") <|>
    route [ ("open/:time", listOpenLocs)
          , ("hours/:location/:time", listHoursAtLoc)
          , ("locations", listLocs)
          ] <|>
    dir "static" (serveDirectory ".")

safeGetParam :: MonadSnap f => B.ByteString -> f B.ByteString
safeGetParam param = fromMaybe "" <$> getParam param

-- connectPostgreSQL conninfo :: IO Connection
-- quickQuery' :: Connection -> String -> [SqlValue] -> IO [[SqlValue]]
query :: String -> [SqlValue] -> IO [[SqlValue]]
query q v = do
    conn <- connectPostgreSQL conninfo
    quickQuery' conn q v
--query = connectPostgreSQL conninfo >>= quickQuery' --WRONG


listOpenLocs :: Snap ()
listOpenLocs = do
    time <- safeGetParam "time"
    result <- liftIO $ openAt time
    writeBS result

listHoursAtLoc :: Snap ()
listHoursAtLoc = do
    location <- safeGetParam "location"
    time <- safeGetParam "time"
    result <- liftIO $ hoursAt location time
    writeBS result

listLocs :: Snap ()
listLocs = liftIO locations >>= writeBS

openAt :: B.ByteString -> IO B.ByteString
openAt = undefined

hoursAt :: B.ByteString -> B.ByteString -> IO B.ByteString
hoursAt = undefined

locations :: IO B.ByteString
locations = undefined

listLocsSQL :: IO B.ByteString
listLocsSQL = undefined

--secondsToTime :: TimeZone -> B.ByteString -> LocalTime
--secondsToTime tz s = utcToLocalTime tz time
--    where time = posixSecondsToUTCTime psec
--          psec = fromIntegral sec :: POSIXTime
--          sec  = read s :: Integer

