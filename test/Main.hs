{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test suite.

module Main where

import           Control.Exception (try, bracket)
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.ODBC
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.Printf

--------------------------------------------------------------------------------
-- Tests

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Connectivity" connectivity
  describe "Data retrieval" dataRetrieval

connectivity :: Spec
connectivity = do
  it
    "Connect, no close"
    (do _ <- connectWithString
        shouldBe True True)
  it
    "Connect, explicit close"
    (do c <- connectWithString
        close c
        shouldBe True True)
  it
    "Double close fails"
    (shouldThrow
       (do c <- connectWithString
           close c
           close c)
       (== DatabaseAlreadyClosed))
  it
    "Connect/disconnect loop"
    (do sequence_ [connectWithString >>= close | _ <- [1 :: Int .. 10]]
        shouldBe True True)

dataRetrieval :: Spec
dataRetrieval = do
  it
    "Simple back and forth"
    (do c <- connectWithString
        exec c "DROP TABLE IF EXISTS test"
        exec c "CREATE TABLE test (int integer, text text, bool bit, nt ntext)"
        exec
          c
          "INSERT INTO test VALUES (123, 'abc', 1, 'wib'), (456, 'def', 0, 'wibble'), (NULL, NULL, NULL, NULL)"
        _ <- query c "SELECT * FROM test"
        close c
        shouldBe True True)
  quickCheckIt "integer" IntValue (T.pack . show)
  quickCheckIt "int" IntValue (T.pack . show)
  quickCheckIt "float" DoubleValue (T.pack . printf "%f")
  quickCheckIt "ntext" TextValue showText
  quickCheckIt "text" BytesValue showBytes
  quickCheckIt
    ("nvarchar(" <> T.pack (show maxStringLen) <> ")")
    TextValue
    showText
  quickCheckIt
    ("varchar(" <> T.pack (show maxStringLen) <> ")")
    BytesValue
    showBytes
  quickCheckIt
    "bit"
    BoolValue
    (\case
       True -> "1"
       False -> "0")

--------------------------------------------------------------------------------
-- Combinators

quickCheckIt :: (Show t, Arbitrary t) => Text -> (t -> Value) -> (t -> Text) -> Spec
quickCheckIt typ cons shower =
  around
    (bracket
       (do c <- connectWithString
           exec c "DROP TABLE IF EXISTS test"
           exec c ("CREATE TABLE test (f " <> typ <> ")")
           pure c)
       close)
    (it
       ("QuickCheck type: " <> T.unpack typ)
       (\c ->
          property
            (\input ->
               monadicIO
                 (do let q = "INSERT INTO test VALUES (" <> shower input <> ")"
                     rows <-
                       liftIO
                         (try
                            (do exec c q
                                query c "SELECT * FROM test"))
                     let expected :: Either ODBCException [[Maybe Value]]
                         expected = Right [[Just (cons input)]]
                     monitor
                       (counterexample
                          (show rows ++
                           " should be " ++
                           show expected ++ "\n\nfor query: " <> T.unpack q))
                     assert (rows == expected)))))

--------------------------------------------------------------------------------
-- Helpers

showBytes :: ByteString -> Text
showBytes t = "'" <> T.pack (S8.unpack t) <> "'"

showText :: Text -> Text
showText t = "N'" <> t <> "'"

validTextChar :: Char -> Bool
validTextChar = \c -> c /= '\'' && c /= '\n' && c /= '\r'

--------------------------------------------------------------------------------
-- Constants

maxStringLen :: Int
maxStringLen = 1024

connectWithString :: IO Connection
connectWithString = do
  mconnStr <- lookupEnv "ODBC_TEST_CONNECTION_STRING"
  case mconnStr of
    Nothing ->
      error
        "Need ODBC_TEST_CONNECTION_STRING environment variable.\n\
        \Example:\n\
        \ODBC_TEST_CONNECTION_STRING='DRIVER={ODBC Driver 13 for SQL Server};SERVER=127.0.0.1;Uid=SA;Pwd=Passw0rd;Encrypt=no'"
    Just connStr -> connect (T.pack connStr)

--------------------------------------------------------------------------------
-- Orphan instances

instance Arbitrary Text where
  arbitrary =
    fmap (T.filter validTextChar . T.pack . take maxStringLen) arbitrary

instance Arbitrary ByteString where
  arbitrary =
    fmap
      (S8.filter (\c -> isAscii c && validTextChar c) .
       S8.pack . take maxStringLen)
      arbitrary
