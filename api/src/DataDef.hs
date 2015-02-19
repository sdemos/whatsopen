{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DataDef where

--import Control.Applicative
--import qualified Data.ByteString as B hiding (map)
import Data.Text
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import System.Time.Utils (renderSecs)
import System.Locale
import Data.Aeson
import GHC.Generics
import Servant
-- PostGreSQL
import Database.HDBC
import Data.Convertible
import Data.Typeable

-- Datatype definitions
data Store = Store
    { storeId  :: Int
    , name     :: Text
    , location :: Text
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
    fromText = Just . read . unpack
instance FromText LocalTime where
    fromText = parseTime defaultTimeLocale "%y%m%dT%T" . unpack

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
    safeConvert ((SqlByteString b1):[]) = (return . read . unpack . decodeUtf8) b1
    safeConvert y = convError "Error converting stuff" y
