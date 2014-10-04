{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import qualified Data.ByteString as B hiding (map)
import           Data.Maybe                  (fromMaybe)
import           Control.Monad.Trans         (liftIO)

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

listOpenLocs :: Snap ()
listOpenLocs = do
    time <- safeGetParam "time"
    result <- liftIO $ openAt time
    writeBS result

-- it's cool that this works. 
-- it's unfortunate that this won't work with the calls with multiple arguments
-- which is why I am going to go with the other one, which is equivalent, although wordier
-- but has the same structure as the multiple argument calls will have
--listOpenLocs :: Snap ()
--listOpenLocs = safeGetParam "time" >>= (liftIO . openAt) >>= writeBS

-- this one is literally why they made do notation
listHoursAtLoc :: Snap ()
listHoursAtLoc = do
    location <- safeGetParam "location"
    time <- safeGetParam "time"
    result <- liftIO $ hoursAt location time
    writeBS result

-- this one doesn't use the general layout because it has no arguments
listLocs :: Snap ()
listLocs = liftIO locations >>= writeBS

openAt :: B.ByteString -> IO B.ByteString
openAt = undefined

hoursAt :: B.ByteString -> B.ByteString -> IO B.ByteString
hoursAt = undefined

locations :: IO B.ByteString
locations = undefined
