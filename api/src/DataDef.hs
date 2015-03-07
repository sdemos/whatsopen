{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DataDef where

--import Control.Applicative
--import qualified Data.ByteString as B hiding (map)
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format (formatTime, parseTime)
import System.Time (TimeDiff(..), normalizeTimeDiff, diffClockTimes, ClockTime(..))
import System.Locale
import Data.Aeson (ToJSON, toJSON)
import Text.Blaze
import GHC.Generics (Generic)
import Servant
-- PostGreSQL
import Database.HDBC
import Data.Convertible
import Data.Typeable

-- Datatype definitions
data Store = Store
    { storeId  :: Int
    , name     :: T.Text
    , location :: T.Text
    } deriving (Generic, Typeable, Show)

data Day = Day
    { getDayStore    :: Store
    , getDayHours    :: [Hours]
    } deriving (Generic, Show)

data Hours = Hours
    { getOpenTime  :: TimeOfDay
    , getCloseTime :: TimeOfDay
    } deriving (Generic, Typeable, Show)

-- the store field in open will get named just store because this is the one getting turned into json to get returned
data Open = Open
    { store    :: Store
    , openFor  :: DiffTime
    , openTill :: TimeOfDay
    } deriving (Generic, Show)

instance ToJSON Open
instance ToJSON Store

instance ToJSON TimeOfDay where
    toJSON = toJSON . formatTime defaultTimeLocale "%l:%M%P"
instance ToJSON DiffTime where
    toJSON = toJSON . renderSecs . round

instance FromText TimeOfDay where
    fromText = Just . read . T.unpack
instance FromText LocalTime where
    fromText = parseTime defaultTimeLocale "%y%m%d%H%M" . T.unpack

instance ToMarkup TimeOfDay where
    toMarkup = toMarkup . formatTime defaultTimeLocale "%l:%M%P"
instance ToMarkup DiffTime where
    toMarkup = toMarkup . renderSecs . round

instance Convertible [SqlValue] Store where
    safeConvert ((SqlInteger t1) :
                 (SqlByteString t2) :
                 (SqlByteString t3) :
                 []) = return $ Store (convert t1) (decodeUtf8 t2) (decodeUtf8 t3)
    safeConvert y = convError "Error converting store" y

instance Convertible [SqlValue] Hours where
    safeConvert ((SqlLocalTimeOfDay t1) :
                 (SqlLocalTimeOfDay t2) :
                 []) = return $ Hours t1 t2
    safeConvert y = convError "Error converting hours" y

instance Convertible [SqlValue] (TimeOfDay, TimeOfDay) where
    safeConvert ((SqlByteString b1):[]) = (return . read . T.unpack . decodeUtf8) b1
    safeConvert y = convError "Error converting stuff" y

renderSecs :: Integer -> String
renderSecs i = renderTD $ diffClockTimes (TOD i 0) (TOD 0 0)

{- | Like 'renderSecs', but takes a TimeDiff instead of an integer second
count. -}
renderTD :: TimeDiff -> String
renderTD itd =
    case workinglist of
      [] -> "1m"
      _ -> intercalate " " . map (\(q, s) -> show q ++ [s]) $ workinglist
    where td = normalizeTimeDiff itd
          quantlist = (\(TimeDiff y mo d h m _ _) -> [y, mo, d, h, m]) td
          zippedlist = zip quantlist "yMdhm"
          -- Drop all leading elements that are 0, then take at most 2
          workinglist = take 2 . dropWhile (\(q, _) -> q == 0) $ zippedlist

