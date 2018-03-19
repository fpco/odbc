{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Measure space usage of the odbc library.

module Main where

import           Control.Concurrent.Async
import qualified Data.Text as T
import qualified Database.ODBC.Internal as Internal
import           System.Environment
import           Weigh

-- | Weigh maps.
main :: IO ()
main =
  mainWith
    (do setColumns [Case, Allocated, Max, Live, GCs]
        sequence_
          [ action
            ("Connect/disconnect: " ++ show n ++ " times")
            (runs
               n
               (do c <- connectWithString
                   Internal.close c))
          | n <- [1, 10, 20]
          ]
        sequence_
          [ action
            ("Connect/disconnect: " ++ show n ++ " n threads")
            (replicateConcurrently
               n
               (do c <- connectWithString
                   Internal.close c))
          | n <- [1, 10, 20]
          ])

-- | Run n times.
runs :: Int -> IO () -> IO ()
runs 0 _ = pure ()
runs !n m = m >> runs (n-1) m

connectWithString :: IO Internal.Connection
connectWithString = do
  mconnStr <- lookupEnv "ODBC_TEST_CONNECTION_STRING"
  case mconnStr of
    Nothing ->
      error
        "Need ODBC_TEST_CONNECTION_STRING environment variable.\n\
        \Example:\n\
        \ODBC_TEST_CONNECTION_STRING='DRIVER={ODBC Driver 13 for SQL Server};SERVER=127.0.0.1;Uid=SA;Pwd=Passw0rd;Encrypt=no'"
    Just connStr -> Internal.connect (T.pack connStr)
