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
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.Printf

connectionString :: Text
connectionString =
  "DRIVER={ODBC Driver 13 for SQL Server};SERVER=192.168.99.100;Uid=SA;Pwd=Passw0rd;Encrypt=no"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "Connectivity"
    (do it
          "Connect, no close"
          (do _ <- connect connectionString
              shouldBe True True)
        it
          "Connect, explicit close"
          (do c <- connect connectionString
              close c
              shouldBe True True))
  describe "Data retrieval" dataRetrieval

dataRetrieval :: Spec
dataRetrieval = do
  it
    "Simple back and forth"
    (do c <- connect connectionString
        exec c "DROP TABLE IF EXISTS test"
        exec c "CREATE TABLE test (int integer, text text, bool bit)"
        exec
          c
          "INSERT INTO test VALUES (123, 'abc', 1), (456, 'def', 0), (NULL, NULL, NULL)"
        _ <- query c "SELECT * FROM test"
        close c
        shouldBe True True)
  quickCheckIt
    "bit"
    BoolValue
    (\case
       True -> "1"
       False -> "0")
  quickCheckIt "integer" IntValue (T.pack . show)
  quickCheckIt "int" IntValue (T.pack . show)
  quickCheckIt "float" DoubleValue (T.pack . printf "%f")
  quickCheckIt "nvarchar(1024)" TextValue showText
  quickCheckIt "varchar(1024)" BytesValue showBytes

instance Arbitrary Text where
  arbitrary = fmap (T.filter validTextChar . T.pack . take 1024) arbitrary

instance Arbitrary ByteString where
  arbitrary = fmap (S8.filter (\c -> isAscii c && validTextChar c) . S8.pack . take 1024) arbitrary

quickCheckIt :: (Show t, Arbitrary t) => Text -> (t -> Value) -> (t -> Text) -> Spec
quickCheckIt typ cons shower =
  around
    (bracket
       (do c <- connect connectionString
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

showBytes :: ByteString -> Text
showBytes t = "'" <> T.pack (S8.unpack t) <> "'"

showText :: Text -> Text
showText t = "N'" <> t <> "'"

validTextChar :: Char -> Bool
validTextChar = \c -> c /= '\'' && c /= '\n' && c /= '\r'
