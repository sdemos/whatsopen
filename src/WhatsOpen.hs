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

module WhatsOpen
( server
, WhatsOpenAPI
, whatsOpenAPI
) where

import Data.List
import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (liftIO)
import Data.Functor.Contravariant (contramap)

import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import System.Time (TimeDiff(..), normalizeTimeDiff, diffClockTimes, ClockTime(..))
import Data.Aeson (ToJSON, toJSON)
import Text.Blaze
import Text.Hamlet (hamletFile)
import GHC.Generics (Generic)

-- servant
import Servant
import Servant.HTML.Blaze

-- hasql
import Hasql.Encoders as E
import Hasql.Decoders as D
import Hasql.Query
import Hasql.Connection
import Hasql.Session as S

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
    :<|> serveDirectory "static"

whatsOpenAPI :: Proxy WhatsOpenAPI
whatsOpenAPI = Proxy

data WORoute = Stylesheet | BootstrapCss | BootstrapJs | CSH | SDemos | Github

woUrlRender :: WORoute -> [(T.Text, T.Text)] -> T.Text
woUrlRender Stylesheet _   = "/static/whatsopen.css"
woUrlRender BootstrapCss _ = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/css/bootstrap.min.css"
woUrlRender BootstrapJs _  = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/js/bootstrap.min.js"
woUrlRender CSH _          = "http://csh.rit.edu/"
woUrlRender SDemos _       = "http://sdemos.com/"
woUrlRender Github _       = "https://github.com/sdemos/whatsopen"

data Store = Store
    { storeId  :: UUID
    , name     :: T.Text
    , location :: T.Text
    } deriving (Generic, Show)

encodeStore :: Params Store
encodeStore = contramap storeId (E.value E.int4)
           <> contramap name (E.value E.text)
           <> contramap location (E.value E.text)
decodeStore :: Row Store
decodeStore = Store <$> D.value D.int4
                    <*> D.value D.text
                    <*> D.value D.text

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Generic, Show)

data Day = Day
    { getDayStore :: Store
    , getDay      :: Weekday
    , getDayHours :: [Hours]
    } deriving (Generic, Show, Read)

--encodeDay :: Params Day
--encodeDay = contramap getDayStore encodeStore
--         <> contramap getDay (E.enum show)
--         <> contramap getDayHours (E.array (E. ))
--decodeDay :: Row Day
--decodeDay = Day <$> decodeStore
--                <*> -- this needs to be more powerful.

data Hours = Hours
    { getHoursStore :: Store
    , getOpenTime   :: TimeOfDay
    , getCloseTime  :: TimeOfDay
    } deriving (Generic, Show)

encodeHours :: Params Hours
encodeHours = undefined
decodeHours :: Row Hours
decodeHours = Hours <$> D.value D.time
                    <*> D.value D.time

-- the store field in open will get named just store because this is the one
-- getting turned into json to get returned
data Open = Open
    { store    :: Store
    , openFor  :: DiffTime
    , openTill :: TimeOfDay
    } deriving (Generic, Show)

encodeOpen :: Params Open
encodeOpen = undefined
decodeOpen :: Row Open
decodeOpen = undefined

instance ToJSON Open
instance ToJSON Store

instance ToJSON TimeOfDay where
    toJSON = toJSON . formatTime defaultTimeLocale "%l:%M%P"
instance ToJSON DiffTime where
    toJSON = toJSON . renderSecs . round

instance FromText TimeOfDay where
    fromText = Just . read . T.unpack
instance FromText LocalTime where
    fromText = parseTimeM True defaultTimeLocale "%y%m%d%H%M" . T.unpack

instance ToMarkup TimeOfDay where
    toMarkup = toMarkup . formatTime defaultTimeLocale "%l:%M%P"
instance ToMarkup DiffTime where
    toMarkup = toMarkup . renderSecs . round

instance ToMarkup [Open] where
    toMarkup openStores = $(hamletFile "templates/whatsopen.hamlet") woUrlRender
instance ToMarkup [Store] where
    toMarkup allStores = $(hamletFile "templates/storelist.hamlet") woUrlRender

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

unsafeFromRight :: (Show l) => Either l r -> r
unsafeFromRight (Right r) = r
unsafeFromRight (Left l) = error (show l)

stores :: IO [Store]
stores = fmap unsafeFromRight (fmap unsafeFromRight connection >>= run (query () s))
    where d = rowsList decodeStore
          s = statement "select * from whatsopen.stores" E.unit d False

storeHours :: LocalTime -> Store -> IO Day
storeHours time store = fmap (Day store . unsafeFromRight) (fmap unsafeFromRight connection >>= run q)
    where e = contramap fst (E.value E.timestamp)
           <> contramap snd (contramap storeId (E.value E.int4))
          d = D.rowsList decodeHours
          s = statement "select open,close from whatsopen.get_hours($1, $2)" e d False
          q = query (time, store) s

connection = acquire (settings "localhost" 5432 "whatsopen" "idontknow" "whatsopen")

renderSecs :: Integer -> String
renderSecs = renderTD . flip diffClockTimes (TOD 0 0) . flip TOD 0

{- | Like 'renderSecs', but takes a TimeDiff instead of an integer second
count. -}
renderTD :: TimeDiff -> String
renderTD itd =
    case workinglist of
      [] -> "<1m"
      _ -> unwords . map (\(q, s) -> show q ++ [s]) $ workinglist
    where td = normalizeTimeDiff itd
          quantlist = (\(TimeDiff y mo d h m _ _) -> [y, mo, d, h, m]) td
          zippedlist = zip quantlist "yMdhm"
          -- Drop all leading elements that are 0, then take at most 2
          workinglist = take 2 . dropWhile (\(q, _) -> q == 0) $ zippedlist

