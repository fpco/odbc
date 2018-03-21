{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Measure space usage of the odbc library.

module Main where

import           Control.Concurrent.Async
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Database.ODBC.Internal as Internal
import qualified Database.ODBC.SQLServer as SQLServer
import           System.Environment
import           Weigh

-- | Weigh maps.
main :: IO ()
main = do
  mlabels <- lookupEnv "ODBC_WEIGH_TESTS"
  case mlabels of
    Nothing ->
      mainWith
        (do setColumns [Case, Allocated, Max, Live, GCs]
            (mapM_ snd tests))
    Just labels ->
      mainWith
        (do setColumns [Case, Allocated, Max, Live, GCs]
            (mapM_ snd (filter ((flip elem (words labels)) . fst) tests)))

tests :: [(String, Weigh ())]
tests = [("connection", connection)
        ,("querying", querying)]

querying :: Weigh ()
querying =
  sequence_
    [ action
      ("Query: " ++ show n ++ " times")
      (runs
         n
         (do c <- connectWithString
             _ <-
               SQLServer.query c "SELECT 12345678, N'Hello, World!'" :: IO [( Int
                                                                            , Text)]
             Internal.close c))
    | n <- [1, 10, 20]
    ]

connection :: Weigh ()
connection = do
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
    ]

-- | Run n times.
runs :: Int -> IO () -> IO ()
runs 0 _ = pure ()
runs !n m = m >> runs (n-1) m

connectWithString :: IO Internal.Connection
connectWithString = do
  mconnStr <- lookupEnvUnquote "ODBC_TEST_CONNECTION_STRING"
  case mconnStr of
    Nothing ->
      error
        "Need ODBC_TEST_CONNECTION_STRING environment variable.\n\
        \Example:\n\
        \ODBC_TEST_CONNECTION_STRING='DRIVER={ODBC Driver 13 for SQL Server};SERVER=127.0.0.1;Uid=SA;Pwd=Passw0rd;Encrypt=no'"
    Just connStr -> Internal.connect (T.pack connStr)

-- | I had trouble passing in environment variables via Docker on
-- Travis without the value coming in with quotes around it.
lookupEnvUnquote :: String -> IO (Maybe [Char])
lookupEnvUnquote = fmap (fmap strip) . lookupEnv
  where strip = reverse . dropWhile (=='"') . reverse . dropWhile (=='"')
