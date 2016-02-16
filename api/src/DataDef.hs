{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DataDef where

import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.Functor.Contravariant (contramap)
import Control.Monad (replicateM)

import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import System.Time (TimeDiff(..), normalizeTimeDiff, diffClockTimes, ClockTime(..))
import Data.Aeson (ToJSON, toJSON)
import Text.Blaze
import GHC.Generics (Generic)
import Servant

-- hasql
import Hasql.Encoders as E
import Hasql.Decoders as D
import Hasql.Query
import Hasql.Connection
import Hasql.Session as S

-- Datatype definitions
data Store = Store
    { storeId  :: Int32
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
--    , getDay      :: Weekday
    , getDayHours :: [Hours]
    } deriving (Generic, Show)

encodeDay :: Params Day
encodeDay = undefined
--decodeDay :: Row Day
--decodeDay = Day <$> decodeStore
--                <*> -- this needs to be more powerful.

data Hours = Hours
    { getOpenTime  :: TimeOfDay
    , getCloseTime :: TimeOfDay
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

--instance Convertible [SqlValue] Store where
--    safeConvert [ SqlInteger t1
--                , SqlByteString t2
--                , SqlByteString t3
--                ] = return $ Store (convert t1) (decodeUtf8 t2) (decodeUtf8 t3)
--    safeConvert y = convError "Error converting store" y
--
--instance Convertible [SqlValue] Hours where
--    safeConvert [ SqlLocalTimeOfDay t1
--                , SqlLocalTimeOfDay t2
--                ] = return $ Hours t1 t2
--    safeConvert y = convError "Error converting hours" y
--
--instance Convertible [SqlValue] (TimeOfDay, TimeOfDay) where
--    safeConvert [SqlByteString b1] = (return . read . T.unpack . decodeUtf8) b1
--    safeConvert y = convError "Error converting stuff" y

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
renderSecs i = renderTD $ diffClockTimes (TOD i 0) (TOD 0 0)

{- | Like 'renderSecs', but takes a TimeDiff instead of an integer second
count. -}
renderTD :: TimeDiff -> String
renderTD itd =
    case workinglist of
      [] -> "1m"
      _ -> unwords . map (\(q, s) -> show q ++ [s]) $ workinglist
    where td = normalizeTimeDiff itd
          quantlist = (\(TimeDiff y mo d h m _ _) -> [y, mo, d, h, m]) td
          zippedlist = zip quantlist "yMdhm"
          -- Drop all leading elements that are 0, then take at most 2
          workinglist = take 2 . dropWhile (\(q, _) -> q == 0) $ zippedlist

