{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- | A helpful client for debugging connections.

module Main (main) where

import           Data.Time.LocalTime (ZonedTime(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Control.Exception
import qualified Database.ODBC.Internal as ODBC
import           System.Environment
import           System.IO
import           Text.Printf

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [connStr] -> do
      conn <- ODBC.connect (T.pack connStr)
      term<- hIsTerminalDevice stdin
      if term
         then repl conn
         else piped conn
    _ -> error "usage: <connection string>"

-- | Accepts a query/command and prints any results.
repl :: ODBC.Connection -> IO ()
repl c = do
  result <- prompt
  case result of
    Nothing -> pure ()
    Just input -> do
      hSetBuffering stdout LineBuffering
      catch
        (catch
           (do (_, count) <- ODBC.stream c input output (False, 0 :: Int)
               putStrLn ("Rows: " ++ show count))
           (\case
              UserInterrupt -> pure ()
              e -> throwIO e))
        (\(e :: ODBC.ODBCException) -> putStrLn (displayException e))
      repl c

-- | Accepts a single input and prints any results.
piped :: ODBC.Connection -> IO ()
piped c = do
  input <- T.hGetContents stdin
  hSetBuffering stdout LineBuffering
  catch
    (catch
       (do (_, count) <- ODBC.stream c input output (False, 0 :: Int)
           putStrLn ("Rows: " ++ show count))
       (\case
          UserInterrupt -> pure ()
          e -> throwIO e))
    (\(e :: ODBC.ODBCException) -> putStrLn (displayException e))
  repl c

prompt :: IO (Maybe T.Text)
prompt = do
  hSetBuffering stdout NoBuffering
  putStr "> "
  catch (fmap Just T.getLine) (\(_ :: IOException) -> pure Nothing)

output :: (Show b, Num b) => (a, b) -> [(ODBC.Column, ODBC.Value)] -> IO (ODBC.Step (Bool, b))
output (_printedHeaders, count) rowWithHeaders = do
  T.putStrLn
    ("[row " <> T.pack (show count) <> "]\n" <>
     T.unlines
       (map
          (\(name, value) ->
             ODBC.columnName name <> ": " <> T.pack (showColumn value))
          rowWithHeaders))
  pure (ODBC.Continue (True, count + 1))
  where
    showColumn =
      \case
        ODBC.NullValue -> "NULL"
        ODBC.TextValue t -> show t
        ODBC.ByteStringValue bs -> show bs
        ODBC.BinaryValue bs -> show bs
        ODBC.BoolValue b -> show b
        ODBC.DoubleValue d -> printf "%f" d
        ODBC.FloatValue d -> printf "%f" d
        ODBC.IntValue i -> show i
        ODBC.DayValue d -> show d
        ODBC.ByteValue b -> show b
        ODBC.TimeOfDayValue v -> show v
        ODBC.LocalTimeValue v -> show v
        ODBC.ZonedTimeValue lt tz -> show $ ZonedTime lt tz
