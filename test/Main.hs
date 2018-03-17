{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test suite.

module Main where

import           Control.Exception (try, bracket, onException, SomeException)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Functor.Identity
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.ODBC.Conversion (FromValue(..))
import           Database.ODBC.Internal (Value (..), Connection, ODBCException(..), Step(..))
import qualified Database.ODBC.Internal as Internal
import           Database.ODBC.SQLServer (ToSql(..))
import qualified Database.ODBC.SQLServer as SQLServer
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
  describe
    "Database.ODBC.Internal"
    (do describe "Connectivity" connectivity
        describe "Data retrieval" dataRetrieval)
  describe
    "Database.ODBC.SQLServer"
    (describe "Conversion to SQL" conversionTo)

conversionTo :: Spec
conversionTo = do
  quickCheckFromValue @Float "Float" "float"
  quickCheckFromValue @Double "Double" "float"
  quickCheckFromValue @Int "Int" "integer"
  quickCheckFromValue @Bool "Bool" "bit"
  quickCheckFromValue @Text "Text" "ntext"
  quickCheckFromValue @Text "Text" ("nvarchar(" <> (show maxStringLen) <> ")")
  quickCheckFromValue @ByteString "ByteString" "text"
  quickCheckFromValue @ByteString "ByteString" ("varchar(" <>  (show maxStringLen) <> ")")

connectivity :: Spec
connectivity = do
  it
    "Connect, no close"
    (do _ <- connectWithString
        shouldBe True True)
  it
    "Connect, explicit close"
    (do c <- connectWithString
        Internal.close c
        shouldBe True True)
  it
    "Double close fails"
    (shouldThrow
       (do c <- connectWithString
           Internal.close c
           Internal.close c)
       (== DatabaseAlreadyClosed))
  it
    "Connect/disconnect loop"
    (do sequence_ [connectWithString >>= Internal.close | _ <- [1 :: Int .. 10]]
        shouldBe True True)

dataRetrieval :: Spec
dataRetrieval = do
  it
    "Basic sanity check"
    (do c <- connectWithString
        Internal.exec c "DROP TABLE IF EXISTS test"
        Internal.exec
          c
          "CREATE TABLE test (int integer, text text, bool bit, nt ntext, fl float)"
        Internal.exec
          c
          "INSERT INTO test VALUES (123, 'abc', 1, 'wib', 2.415), (456, 'def', 0, 'wibble',0.9999999999999), (NULL, NULL, NULL, NULL, NULL)"
        rows <- Internal.query c "SELECT * FROM test"
        Internal.close c
        shouldBe
          rows
          [ [ Just (IntValue 123)
            , Just (ByteStringValue "abc")
            , Just (BoolValue True)
            , Just (TextValue "wib")
            , Just (DoubleValue 2.415)
            ]
          , [ Just (IntValue 456)
            , Just (ByteStringValue "def")
            , Just (BoolValue False)
            , Just (TextValue "wibble")
            , Just (DoubleValue 0.9999999999999)
            ]
          , [Nothing, Nothing, Nothing, Nothing, Nothing]
          ])
  it
    "Querying commands with no results"
    (do c <- connectWithString
        rows1 <- Internal.query c "DROP TABLE IF EXISTS no_such_table"
        rows2 <-
          Internal.stream
            c
            "DROP TABLE IF EXISTS no_such_table"
            (\s _ -> pure (Stop s))
            []
        shouldBe (rows1 ++ rows2) [])
  quickCheckIt
    "Int"
    "integer"
    (T.pack . show)
    (\case
       IntValue b -> pure b
       _ -> Nothing)
  quickCheckIt
    "Int"
    "int"
    (T.pack . show)
    (\case
       IntValue b -> pure b
       _ -> Nothing)
  quickCheckIt
    "Double"
    "float"
    (T.pack . printf "%f")
    (\case
       DoubleValue b -> pure (realToFrac b :: Double)
       _ -> Nothing)
  quickCheckIt
    "Float"
    "float"
    (T.pack . printf "%f")
    (\case
       DoubleValue b -> pure (realToFrac b :: Float)
       _ -> Nothing)
  quickCheckIt
    "Text"
    "ntext"
    showText
    (\case
       TextValue b -> pure b
       _ -> Nothing)
  quickCheckIt
    "ByteString"
    "text"
    showBytes
    (\case
       ByteStringValue b -> pure b
       _ -> Nothing)
  quickCheckIt
    "Text"
    ("nvarchar(" <> T.pack (show maxStringLen) <> ")")
    showText
    (\case
       TextValue b -> pure b
       _ -> Nothing)
  quickCheckIt
    "ByteString"
    ("varchar(" <> T.pack (show maxStringLen) <> ")")
    showBytes
    (\case
       ByteStringValue b -> pure b
       _ -> Nothing)
  quickCheckIt
    "Bool"
    "bit"
    (\case
       True -> "1"
       False -> "0")
    (\case
       BoolValue b -> pure b
       _ -> Nothing)

--------------------------------------------------------------------------------
-- Combinators

quickCheckFromValue ::
     forall t. (Arbitrary t, Eq t, Show t, ToSql t, FromValue t) => String -> String -> Spec
quickCheckFromValue l typ =
  around
    (bracket
       (do c <- connectWithString
           SQLServer.exec c "DROP TABLE IF EXISTS test"
           SQLServer.exec c ("CREATE TABLE test (f " <> fromString typ <> ")")
           pure c)
       SQLServer.close)
    (it
       ("QuickCheck roundtrip: HS=" <> l <> ", SQL="  <> typ)
       (\c ->
          property
            (\input ->
               monadicIO
                 (do let q = "INSERT INTO test VALUES (" <> toSql (input :: t) <> ")"
                     SQLServer.exec c q
                     [Identity result] <- SQLServer.query c "SELECT * FROM test"
                     monitor
                       (counterexample
                          (unlines
                             [ "Expected: " ++ show input
                             , "Actual: " ++ show result
                             , "Query was: " ++ show q
                             ]))
                     assert (result == input)))))

quickCheckIt ::
     forall t. (Eq t, Show t, Arbitrary t)
  => Text
  -> Text
  -> (t -> Text)
  -> (Value -> Maybe t)
  -> Spec
quickCheckIt hstype typ shower unpack =
  around
    (bracket
       (do c <- connectWithString
           Internal.exec c "DROP TABLE IF EXISTS test"
           Internal.exec c ("CREATE TABLE test (f " <> typ <> ")")
           pure c)
       Internal.close)
    (it
       ("QuickCheck roundtrip: HS=" <> T.unpack  hstype <> ", SQL="  <> T.unpack typ)
       (\c ->
          property
            (\input ->
               monadicIO
                 (do let q = "INSERT INTO test VALUES (" <> shower input <> ")"
                     rows <-
                       liftIO
                         (try
                            (do onException (Internal.exec c q) (putStrLn "Exec failed.")
                                onException
                                  (Internal.query c "SELECT * FROM test")
                                  (putStrLn "Query failed!")))
                     let expected :: Either String t
                         expected = Right input
                         result :: Either String t
                         result =
                           case rows of
                             Right [[Just x]] ->
                               case unpack x of
                                 Nothing -> Left "Couldn't unpack value."
                                 Just v -> pure v
                             Right _ -> Left "Invalid number of values returned."
                             Left (_ :: SomeException) ->
                               Left "Couldn't get value from row in test suite."
                     when
                       (result /= expected)
                       (liftIO
                          (putStr
                             (unlines
                                [ "Expected: " ++ show expected
                                , "Actual: " ++ show rows
                                , "Query was: " ++ show q
                                , "QuickCheck value: " ++ show input
                                ])))
                     monitor
                       (counterexample
                          (unlines
                             [ "Expected: " ++ show expected
                             , "Actual: " ++ show rows
                             , "Query was: " ++ show q
                             ]))
                     assert (result == expected)))))

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
    Just connStr -> Internal.connect (T.pack connStr)

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
