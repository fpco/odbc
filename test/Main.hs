{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
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

module Main (main) where

import           Control.Exception (try, onException, SomeException, catch, throwIO)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Functor.Identity
import           Data.Int
import           Data.Ratio
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Word
import           Database.ODBC.Conversion (FromValue(..))
import           Database.ODBC.Internal (Value (..), Connection, ODBCException(..), Step(..), Binary)
import qualified Database.ODBC.Internal as Internal
import           Database.ODBC.SQLServer (splitQueryParametrized, joinQueryParametrized, Datetime2(..), Datetimeoffset(..), Smalldatetime(..), ToSql(..))
import qualified Database.ODBC.SQLServer as SQLServer
import           Database.ODBC.TH (partsParser, Part(..))
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import qualified Text.Parsec as Parsec
import           Text.Printf

--------------------------------------------------------------------------------
-- Tests

main :: IO ()
main = do
  mconnStr <- lookupEnvUnquote "ODBC_TEST_CONNECTION_STRING"
  putStrLn ("Using connection string: " ++ show mconnStr)
  hspec spec

spec :: Spec
spec = do
  describe
    "Database.ODBC.Internal"
    (do describe "Connectivity" connectivity
        describe "Regression tests" regressions
        describe "Data retrieval" dataRetrieval
        describe "Data affected" dataAffected
        describe "Big data" bigData)
  describe
    "Database.ODBC.SQLServer"
    (do describe "Conversion to SQL" conversionTo
        describe "Parametrized" parametrizedSpec)
  describe "Database.ODBC.TH" thparser

thparser :: SpecWith ()
thparser =
  describe
    "Parser"
    (do it
          "Basic range of syntax"
          (shouldBe
             (Parsec.parse partsParser "" "foo $bar3*123 zot '$$12.4' $wibble")
             (Right
                [ SqlPart "foo "
                , ParamName "bar3"
                , SqlPart "*123 zot '"
                , SqlPart "$"
                , SqlPart "12.4' "
                , ParamName "wibble"
                ]))
        it
          "Incomplete parameter"
          (shouldBe
             (first (const ()) (Parsec.parse partsParser "" "foo$"))
             (Left ()))
        it
          "Invalid parameter character"
          (shouldBe
             (first (const ()) (Parsec.parse partsParser "" "foo$."))
             (Left ())))

regressions :: Spec
regressions = do
  it
    "Internal.exec can return SQL_NO_DATA"
    (do c <- connectWithString
        Internal.exec c "DROP TABLE IF EXISTS wibble"
        Internal.exec c "CREATE TABLE wibble (i integer)"
        Internal.exec c "DELETE FROM wibble"
        Internal.close c)
  it
    "Internal.exec multiple statements (https://github.com/fpco/odbc/issues/9)"
    (do c <- connectWithString
        Internal.exec c "SELECT 1; SELECT 2"
        Internal.close c)
  it
    "Internal.exec error in multiple statements (https://github.com/fpco/odbc/issues/9)"
    (do c <- connectWithString
        shouldThrow
          (Internal.exec c "SELECT 1; SELECT nothing FROM doesntexist")
          (\case
             Internal.UnsuccessfulReturnCode "odbc_SQLMoreResults" (-1) _ _ ->
               True
             _ -> False))

-- | Test fields with large data like megabytes of text. Just to check
-- we don't have some kind of hard limit problem.
bigData :: Spec
bigData = do
  roundtrip @Text "2MB text" "Text" "ntext" (T.replicate (1024*1024*2) "A")
  roundtrip @ByteString "2MB binary" "ByteString" "text" (S.replicate (1024*1024*2) 97)
  roundtrip @Binary "2MB binary" "Binary" "varbinary(max)" (SQLServer.Binary (S.replicate 10 97))

conversionTo :: Spec
conversionTo = do
  -- See <https://docs.microsoft.com/en-us/sql/t-sql/data-types/int-bigint-smallint-and-tinyint-transact-sql>
  describe
    "maxBound"
    (do roundtrip @Int "maxBound(Int64)" "Int" "bigint" maxBound
        roundtrip @Int "maxBound(Int32)" "Int" "int" (fromIntegral (maxBound :: Int32))
        roundtrip @Int "maxBound(Int16)" "Int" "smallint" (fromIntegral (maxBound :: Int16))
        roundtrip @Word8 "maxBound(Word8)" "Word8" "tinyint" maxBound)
  describe
    "minBound"
    (do roundtrip @Int "minBound(Int64)" "Int" "bigint" minBound
        roundtrip @Int "minBound(Int32)" "Int" "int" (fromIntegral (minBound :: Int32))
        roundtrip @Int "minBound(Int16)" "Int" "smallint" (fromIntegral (minBound :: Int16))
        roundtrip @Word8 "minBound(Word8)" "Word8" "tinyint" minBound)
  quickCheckRoundtrip @Day "Day" "date"
  quickCheckRoundtrip @Datetime2 "Datetime2" "datetime2"
  quickCheckRoundtrip @Smalldatetime "Smalldatetime" "smalldatetime"
  quickCheckRoundtrip @TestDateTime "TestDateTime" "datetime"
  quickCheckRoundtrip @Datetimeoffset "Datetimeoffset" "datetimeoffset"
  quickCheckOneway @TimeOfDay "TimeOfDay" "time"
  quickCheckRoundtrip @TestTimeOfDay "TimeOfDay" "time"
  quickCheckRoundtrip @Float "Float" "real"
  quickCheckRoundtrip @Double "Double" "float"
  quickCheckRoundtrip @Decimal "Double" ("decimal(" <> show decimalPrecision <> ", " <> show decimalScale <> ")")
  quickCheckRoundtrip @Decimal "Double" ("numeric(" <> show decimalPrecision <> ", " <> show decimalScale <> ")")
  quickCheckRoundtrip @Double "Float" "float"
  quickCheckRoundtrip @Word8 "Word8" "tinyint"
  quickCheckRoundtrip @Int "Int" "bigint"
  quickCheckRoundtrip @Bool "Bool" "bit"
  quickCheckRoundtrip @Text "Text" "ntext"
  quickCheckRoundtrip @Text "Text" ("nvarchar(" <> (show maxStringLen) <> ")")
  quickCheckRoundtrip @ByteString "ByteString" "text"
  quickCheckRoundtrip @TestChar "ByteString" ("char(" <>  (show maxStringLen) <> ")")
  quickCheckRoundtrip @ByteString "ByteString" ("varchar(" <>  (show maxStringLen) <> ")")
  quickCheckRoundtrip @TestBinary "ByteString" ("binary(" <>  (show maxStringLen) <> ")")
  quickCheckRoundtrip @Binary "ByteString" ("varbinary(" <>  (show maxStringLen) <> ")")
  quickCheckRoundtrip @TestGUID "GUID" "uniqueidentifier" -- Regression tests against https://github.com/fpco/odbc/issues/30

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

dataAffected :: Spec
dataAffected = do
  it
    "Basic sanity check"
    (do c <- connectWithString
        _ <- Internal.execAffectedRows c "DROP TABLE IF EXISTS test"
        arOnCreate <- Internal.execAffectedRows
          c
          "CREATE TABLE test (int integer, text text, bool bit, nt ntext, fl float)"
        _ <- Internal.execAffectedRows
          c
          "INSERT INTO test VALUES (123, 'abc', 1, 'wib', 2.415), (456, 'def', 0, 'wibble',0.9999999999999), (NULL, NULL, NULL, NULL, NULL)"
        arOnDelete <- Internal.execAffectedRows c "delete from test"
        arOnDelete' <- Internal.execAffectedRows c "delete from test"
        Internal.close c
        shouldBe
          [("create", arOnCreate), ("delete", arOnDelete), ("delete'", arOnDelete')]
          [("create", 0), ("delete", 3), ("delete'", 0)])

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
          (map (map snd) rows)
          [ [  (IntValue 123)
            ,  (ByteStringValue "abc")
            ,  (BoolValue True)
            ,  (TextValue "wib")
            ,  (DoubleValue 2.415)
            ]
          , [  (IntValue 456)
            ,  (ByteStringValue "def")
            ,  (BoolValue False)
            ,  (TextValue "wibble")
            ,  (DoubleValue 0.9999999999999)
            ]
          , [NullValue, NullValue, NullValue, NullValue, NullValue]
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
        shouldBe (map (map snd) (rows1 ++ rows2)) [])
  quickCheckInternalRoundtrip
    "Int"
    "bigint"
    (T.pack . show)
    (\case
       IntValue b -> pure b
       _ -> Nothing)
  quickCheckInternalRoundtrip
    "Double"
    "float"
    (T.pack . printf "%f")
    (\case
       DoubleValue b -> pure (realToFrac b :: Double)
       _ -> Nothing)
  quickCheckInternalRoundtrip
    "Float"
    "float"
    (T.pack . printf "%f")
    (\case
       DoubleValue b -> pure (realToFrac b :: Float)
       _ -> Nothing)
  quickCheckInternalRoundtrip
    "Float"
    "real"
    (T.pack . printf "%f")
    (\case
       FloatValue b -> pure b
       _ -> Nothing)
  quickCheckInternalRoundtrip
    "Text"
    "ntext"
    showText
    (\case
       TextValue b -> pure b
       _ -> Nothing)
  quickCheckInternalRoundtrip
    "Text"
    ("nvarchar(" <> T.pack (show maxStringLen) <> ")")
    showText
    (\case
       TextValue b -> pure b
       _ -> Nothing)
  quickCheckInternalRoundtrip
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

roundtrip ::
     forall t. (Eq t, Show t, ToSql t, FromValue t)
  => String
  -> String
  -> String
  -> t
  -> Spec
roundtrip why l typ input =
  it
    ("Roundtrip " <> why <> ": HS=" <> l <> ", SQL=" <> typ)
    (do c <- connectWithString
        SQLServer.exec c "DROP TABLE IF EXISTS test"
        SQLServer.exec c ("CREATE TABLE test (f " <> fromString typ <> ")")
        let q = "INSERT INTO test VALUES (" <> toSql (input) <> ")"
        SQLServer.exec c q
        [Identity (!result)] <- SQLServer.query c "SELECT * FROM test"
        SQLServer.close c
        shouldBe result input)

quickCheckRoundtrip ::
     forall t. (Arbitrary t, Eq t, Show t, ToSql t, FromValue t, ToSql (Maybe t))
  => String
  -> String
  -> Spec
quickCheckRoundtrip = quickCheckRoundtripEx @t True

quickCheckRoundtripEx ::
     forall t.
     (Arbitrary t, Eq t, Show t, ToSql t, FromValue t, ToSql (Maybe t))
  => Bool
  -> String
  -> String
  -> Spec
quickCheckRoundtripEx testMaybes l typ =
  beforeAll
    (do c <- connectWithString
        SQLServer.exec c "DROP TABLE IF EXISTS test"
        SQLServer.exec c ("CREATE TABLE test (f " <> fromString typ <> ")")
        pure c)
    (afterAll
       SQLServer.close
       (let makeIt ::
                 forall t'.
                 ( Arbitrary t'
                 , Show t'
                 , ToSql t'
                 , FromValue t'
                 , Eq t'
                 , ToSql (Maybe t')
                 )
              => String
              -> String
              -> (t -> t')
              -> SpecWith Connection
            makeIt l' typ' f =
              it
                ("QuickCheck roundtrip: HS=" <> l' <> ", SQL=" <> typ')
                (\c ->
                   property
                     (\(f -> input) ->
                        monadicIO
                          (do SQLServer.exec c "TRUNCATE TABLE test"
                              let q =
                                    "INSERT INTO test VALUES (" <> toSql input <>
                                    ")"
                              liftIO
                                (catch
                                   (SQLServer.exec c q)
                                   (\e -> do
                                      print (e :: SomeException)
                                      T.putStrLn (SQLServer.renderQuery q)
                                      SQLServer.close c
                                      throwIO e))
                              [Identity result] <-
                                SQLServer.query c "SELECT f FROM test"
                              when
                                (result /= input)
                                (liftIO
                                   (putStr
                                      (unlines
                                         [ "Expected: " ++ take 80 (show input)
                                         , "Actual: " ++ take 80 (show result)
                                         , "Query was: " ++ show q
                                         ])))
                              monitor
                                (counterexample
                                   (unlines
                                      [ "Expected: " ++ take 80 (show input)
                                      , "Actual: " ++ take 80 (show result)
                                      , "Query was: " ++
                                        T.unpack (SQLServer.renderQuery q)
                                      ]))
                              assert (result == input))))
         in do makeIt l typ id
               when
                 testMaybes
                 (do makeIt ("Maybe " ++ l) typ Just
                     makeIt
                       ("Maybe " ++ l)
                       (typ ++ " (NULL)")
                       (const Nothing :: a -> Maybe a))))

quickCheckOneway ::
     forall t. (Arbitrary t, Eq t, Show t, ToSql t, FromValue t)
  => String
  -> String
  -> Spec
quickCheckOneway l typ =
  beforeAll
    (do c <- connectWithString
        SQLServer.exec c "DROP TABLE IF EXISTS test"
        SQLServer.exec c ("CREATE TABLE test (f " <> fromString typ <> ")")
        pure c)
    (afterAll
       SQLServer.close
       (it
          ("QuickCheck one-way: HS=" <> l <> ", SQL=" <> typ)
          (\c ->
             property
               (\input ->
                  monadicIO
                    (do (let q = "TRUNCATE TABLE test"
                         in liftIO
                              (onException
                                 (SQLServer.exec c q)
                                 ((T.putStrLn (SQLServer.renderQuery q)))))
                        let q =
                              "INSERT INTO test VALUES (" <> toSql (input :: t) <>
                              ")"
                        liftIO
                          (onException
                             (SQLServer.exec c q)
                             ((T.putStrLn (SQLServer.renderQuery q))))
                        [Identity result] <-
                          SQLServer.query c "SELECT f FROM test"
                        monitor
                          (counterexample
                             (unlines
                                [ "Expected: " ++ show input
                                , "Actual: " ++ show (result :: t)
                                , "Query was: " ++
                                  T.unpack (SQLServer.renderQuery q)
                                ]))
                        assert True)))))

quickCheckInternalRoundtrip ::
     forall t. (Eq t, Show t, Arbitrary t)
  => Text
  -> Text
  -> (t -> Text)
  -> (Value -> Maybe t)
  -> Spec
quickCheckInternalRoundtrip hstype typ shower unpack =
  beforeAll
    (do c <- connectWithString
        Internal.exec c "DROP TABLE IF EXISTS test"
        Internal.exec c ("CREATE TABLE test (f " <> typ <> ")")
        pure c)
    (afterAll
       Internal.close
       (it
          ("QuickCheck roundtrip: HS=" <> T.unpack hstype <> ", SQL=" <>
           T.unpack typ)
          (\c ->
             property
               (\input ->
                  monadicIO
                    (do Internal.exec c "TRUNCATE TABLE test"
                        let q =
                              "INSERT INTO test VALUES (" <> shower input <> ")"
                        rows <-
                          liftIO
                            (try
                               (do onException
                                     (Internal.exec c q)
                                     (putStrLn "Exec failed.")
                                   onException
                                     (Internal.query c "SELECT * FROM test")
                                     (putStrLn "Query failed!")))
                        let expected :: Either String t
                            expected = Right input
                            result :: Either String t
                            result =
                              case rows of
                                Right [[(_,x)]] ->
                                  case unpack x of
                                    Nothing -> Left "Couldn't unpack value."
                                    Just v -> pure v
                                Right _ ->
                                  Left "Invalid number of values returned."
                                Left (_ :: SomeException) ->
                                  Left
                                    "Couldn't get value from row in test suite."
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
                        assert (result == expected))))))

--------------------------------------------------------------------------------
-- Helpers

showText :: Text -> Text
showText t = "N'" <> t <> "'"

validTextChar :: Char -> Bool
validTextChar =
  \c -> c /= '\'' && c /= '\n' && c /= '\r' && doesNotExceedWideChar c
  where
    doesNotExceedWideChar c = c < toEnum (fromIntegral (maxBound :: Word16))

--------------------------------------------------------------------------------
-- Constants

maxStringLen :: Int
maxStringLen = 1024

connectWithString :: IO Connection
connectWithString = do
  mconnStr <- lookupEnvUnquote "ODBC_TEST_CONNECTION_STRING"
  case mconnStr of
    Just connStr
      | not (null connStr) -> Internal.connect (T.pack connStr)
    _ ->
      error
        "Need ODBC_TEST_CONNECTION_STRING environment variable.\n\
        \Example:\n\
        \ODBC_TEST_CONNECTION_STRING='DRIVER={ODBC Driver 17 for SQL Server};SERVER=127.0.0.1;Uid=SA;Pwd=Passw0rd;Encrypt=no'"

-- | I had trouble passing in environment variables via Docker on
-- Travis without the value coming in with quotes around it.
lookupEnvUnquote :: String -> IO (Maybe [Char])
lookupEnvUnquote = fmap (fmap strip) . lookupEnv
  where strip = reverse . dropWhile (=='"') . reverse . dropWhile (=='"')

--------------------------------------------------------------------------------
-- Parametrized queries

parametrizedSpec :: Spec
parametrizedSpec = do
  it
    "splitQueryParametrized"
    (do shouldBe (splitQueryParametrized "select 123") ("select 123", [])
        shouldBe
          (splitQueryParametrized ("select " <> toSql (123 :: Int)))
          ("select ?", [IntValue 123])
        shouldBe
          (splitQueryParametrized
             ("select " <> toSql (123 :: Int) <> " from y where x = " <>
              toSql ("abc" :: Text)))
          ("select ? from y where x = ?", [IntValue 123, TextValue "abc"]))
  it
    "joinQueryParametrized"
    (do shouldBe
          (uncurry joinQueryParametrized ("?select 123", []))
          (Left "SQL text without ?: Failed reading: takeWhile1")
        shouldBe
          (uncurry joinQueryParametrized ("select 123", []))
          (Right "select 123")
        shouldBe
          (uncurry
             joinQueryParametrized
             ("select ? from y where x = ?", [IntValue 123, TextValue "abc"]))
          (Right
             ("select " <> toSql (123 :: Int) <> " from y where x = " <>
              toSql ("abc" :: Text)))
        shouldBe
          (uncurry
             joinQueryParametrized
             ("select ? from y where x = ", [IntValue 123, TextValue "abc"]))
          (Left "not enough ? or extraneous param")
        shouldBe
          (uncurry
             joinQueryParametrized
             ("select ? from y where x = ?", [IntValue 123]))
          (Left "too many ? in format string or missing param"))
  beforeAll
    connectWithString
    (afterAll
       Internal.close
       (it
          "Multiple params"
          (\c ->
             property
               (\text bytes -> do
                  rows :: [[Value]] <-
                    SQLServer.query
                      c
                      ("SELECT " <> toSql (text :: Text) <> ",  " <>
                       toSql (SQLServer.Binary bytes :: Binary))
                  shouldBe
                    rows
                    [ [ TextValue text
                      , BinaryValue (SQLServer.Binary bytes)
                      ]
                    ]))))

--------------------------------------------------------------------------------
-- Orphan instances

instance Arbitrary Text where
  arbitrary =
    fmap (T.filter validTextChar . T.pack . take maxStringLen) arbitrary

instance Arbitrary ByteString where
  arbitrary =
    fmap
      (S8.filter (\c -> validTextChar c) .
       S8.pack . take maxStringLen)
      arbitrary

instance Arbitrary Binary where
  arbitrary = fmap SQLServer.Binary arbitrary

newtype TestBinary = TestBinary Binary
  deriving (Show, Eq, Ord, SQLServer.ToSql, FromValue)

instance Arbitrary TestBinary where
  arbitrary = do
    bytes <- arbitrary
    pure
      (TestBinary
         (SQLServer.Binary
            (S.take maxStringLen (bytes <> S.replicate maxStringLen 0))))

newtype TestGUID = TestGUID Binary
  deriving (Show, Eq, Ord, SQLServer.ToSql, FromValue)

instance Arbitrary TestGUID where
  arbitrary = do
    bytes <- arbitrary
    pure
      (TestGUID
         (SQLServer.Binary
            (S.take 16 (bytes <> S.replicate maxStringLen 0))))

-- | The maximum total number of decimal digits that will be stored,
-- both to the left and to the right of the decimal point. The
-- precision must be a value from 1 through the maximum precision of
-- 38. The default precision is 18.
decimalPrecision :: Int
decimalPrecision = 20

-- | The number of decimal digits that will be stored to the right of
-- the decimal point. This number is subtracted from p to determine
-- the maximum number of digits to the left of the decimal point. The
-- maximum number of decimal digits that can be stored to the right of
-- the decimal point. Scale must be a value from 0 through p. Scale
-- can be specified only if precision is specified. The default scale
-- is 0; therefore, 0 <= s <= p. Maximum storage sizes vary, based on
-- the precision.
decimalScale :: Int
decimalScale = 10

newtype Decimal = Decimal Double
  deriving (Show, Eq, Ord, SQLServer.ToSql, FromValue)

instance Arbitrary Decimal where
  arbitrary = do
    left :: Int <- choose (minBound,maxBound)
    right :: Int <- choose (minBound,maxBound)
    pure
      (Decimal
         (read (take leftLen (show left) ++ "." ++ take rightLen (show (abs right)))))
    where
      leftLen = decimalPrecision - decimalScale
      rightLen = decimalScale

newtype TestChar = TestChar ByteString
  deriving (Show, Eq, Ord, SQLServer.ToSql, FromValue)

instance Arbitrary TestChar where
  arbitrary = do
    t <-
      fmap
        (S8.filter (\c -> isAscii c && validTextChar c) .
         S8.pack . take maxStringLen)
        arbitrary
    pure
      (TestChar
         (S.take
            maxStringLen
            ((if S.null t
                then "a"
                else t) <>
             S8.replicate maxStringLen ' ')))

instance Arbitrary Day where
  arbitrary = do
    offset <- choose (0, 30000)
    pure (addDays offset (fromGregorian 1753 01 01))

instance Arbitrary TimeOfDay where
  arbitrary = do
    fractional <- choose (0, 9999999) :: Gen Integer
    seconds <- choose (0, 86400)
    pure
      (timeToTimeOfDay
         (secondsToDiffTime seconds + (fromRational (fractional % 10000000))))

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitraryLimited
    where
      arbitraryLimited = do
        fractional <- choose (0, 9999999) :: Gen Integer
        seconds <- choose (0, 86400)
        pure
          (timeToTimeOfDay
             (secondsToDiffTime seconds + (fromRational (fractional % 10000000))))

newtype TestDateTime = TestDateTime Datetime2
  deriving (Eq, Ord, Show, SQLServer.ToSql, FromValue)

newtype TestTimeOfDay = TestTimeOfDay TimeOfDay
  deriving (Eq, Ord, Show, SQLServer.ToSql, FromValue)

instance Arbitrary TestTimeOfDay where
  arbitrary = do
    seconds <- choose (0, 86400)
    pure (TestTimeOfDay (timeToTimeOfDay (secondsToDiffTime seconds)))

instance Arbitrary TestDateTime where
  arbitrary = fmap (TestDateTime . Datetime2) (LocalTime <$> arbitrary <*> arbitraryLimited)
    where
      arbitraryLimited = do
        fractional <- elements [993, 003, 497, 007, 000, 127] :: Gen Integer
        -- 	Rounded to increments of .000, .003, or .007 seconds
        -- from: https://docs.microsoft.com/en-us/sql/t-sql/data-types/datetime-transact-sql
        seconds <- choose (0, 86400)
        pure
          (timeToTimeOfDay
             (secondsToDiffTime seconds + (fromRational (fractional % 1000))))

deriving instance Arbitrary Datetime2
instance Arbitrary Smalldatetime where
  arbitrary = do
    minutes <- choose (0, 1440)
    day <-
      do offset <- choose (0, 179)
         pure (addDays offset (fromGregorian 1900 01 01))
    pure
      (Smalldatetime
         (LocalTime day (timeToTimeOfDay (secondsToDiffTime (minutes * 60)))))

instance Arbitrary Datetimeoffset where
  arbitrary = do
    lt <- arbitrary
    -- Pick a time zone offset between -12 hours and +14 hours. According to
    -- https://en.wikipedia.org/wiki/List_of_UTC_time_offsets the lowest offset
    -- is -12 hours (at Baker Island and Howland Island), while the highest
    -- offset is +14 hours (at Line Islands).
    offset <- choose (-12 * 60, 14 * 60)
    return $ Datetimeoffset $ ZonedTime lt $ TimeZone offset False ""
