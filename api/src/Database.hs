{-# LANGUAGE FlexibleContexts #-}
module Database
    ( query
    , query_
    ) where

import Control.Applicative
import Data.Convertible (Convertible(..), convert)
import Database.HDBC hiding (run)
import Database.HDBC.PostgreSQL

import Config

connection :: IO Connection
connection = connectPostgreSQL conninfo

query :: Convertible [SqlValue] b => String -> [SqlValue] -> IO [b]
query q p = do
    conn <- connection
    map convert <$> quickQuery conn q p

query_ :: Convertible [SqlValue] b => String -> IO [b]
query_ = flip query []

