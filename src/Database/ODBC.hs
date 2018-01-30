{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | ODBC database API.

module Database.ODBC
  (
    -- * Connection
    connect
  , close
    -- * Executing queries
  , exec
  , query
    -- * Types
  , Value(..)
  , Connection
  , ODBCException(..)
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import           Data.Coerce
import           Data.Data
import           Data.Int
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Foreign as T
import           Foreign
import           Foreign.C
import           GHC.Generics

--------------------------------------------------------------------------------
-- Public types

-- | Connection to a database.
newtype Connection = Connection
  {connectionMVar :: MVar (Maybe (ForeignPtr EnvAndDbc))}

-- | A database exception.
data ODBCException
  = UnsuccessfulReturnCode !String
                           !Int16
    -- ^ An ODBC operation failed with the given return code.
  | AllocationReturnedNull !String
    -- ^ Allocating an ODBC resource failed.
  | UnknownDataType !String
                    !Int16
    -- ^ An unsupported/unknown data type was returned from the ODBC
    -- driver.
  | DatabaseIsClosed !String
    -- ^ You tried to use the database connection after it was closed.
  | DatabaseAlreadyClosed
    -- ^ You attempted to 'close' the database twice.
  deriving (Typeable, Show, Eq)
instance Exception ODBCException

-- | A value used for input/output with the database.
data Value
  = TextValue !Text
    -- ^ A Unicode text value.
  | BytesValue !ByteString
    -- ^ A vector of bytes. It might be a string, but we don't know
    -- the encoding.
  | BoolValue !Bool
    -- ^ A simple boolean.
  | DoubleValue !Double
    -- ^ Floating point values that fit in a 'Double'.
  | IntValue !Int
    -- ^ Integer values that fit in an 'Int'.
  deriving (Eq, Show, Typeable, Ord, Generic, Data)
instance NFData Value

--------------------------------------------------------------------------------
-- Internal types

-- | A column description.
data Column = Column
  { columnType :: !SQLSMALLINT
  , columnSize :: !SQLULEN
  , columnDigits :: !SQLSMALLINT
  , columnNull :: !SQLSMALLINT
  } deriving (Show)

--------------------------------------------------------------------------------
-- Exposed functions

-- | Connect using the given connection string.
connect ::
     MonadIO m
  => Text -- ^ Connection string.
  -> m Connection
connect string =
  withBound
    (do envAndDbc <-
          uninterruptibleMask_
            (do ptr <-
                  assertNotNull "odbc_AllocEnvAndDbc" odbc_AllocEnvAndDbc
                newForeignPtr odbc_FreeEnvAndDbc (coerce ptr))
             -- Above: Allocate the environment.
             -- Below: Try to connect to the database.
        T.useAsPtr
          string
          (\wstring len ->
             uninterruptibleMask_
               (do assertSuccess
                     "odbc_SQLDriverConnectW"
                     (withForeignPtr
                        envAndDbc
                        (\dbcPtr ->
                           odbc_SQLDriverConnectW
                             dbcPtr
                             (coerce wstring)
                             (fromIntegral len)))
                   addForeignPtrFinalizer odbc_SQLDisconnect envAndDbc))
             -- Below: Keep the environment and the database handle in an mvar.
        mvar <- newMVar (Just envAndDbc)
        pure (Connection mvar))

-- | Close the connection. Further use of the 'Connection' will throw
-- an exception. Double closes also throw an exception to avoid
-- architectural mistakes.
close :: MonadIO m => Connection -> m ()
close conn =
  withBound
    (do mstate <- modifyMVar (connectionMVar conn) (pure . (Nothing, ))
        -- If an async exception comes after here, that's a pity
        -- because we wanted to free the connection now. But with
        -- regards to safety, the finalizers will take care of closing
        -- the connection and the env.
        maybe (throwIO DatabaseAlreadyClosed) finalizeForeignPtr mstate)

-- | Execute a statement on the database.
exec ::
     MonadIO m
  => Connection
  -> Text -- ^ SQL statement.
  -> m ()
exec conn string =
  withBound
    (withHDBC conn "exec" (\dbc -> withExecDirect dbc string (const (pure ()))))

-- | Query and return a list of rows.
query ::
     MonadIO m
  => Connection
  -> Text -- ^ SQL Query.
  -> m [[Maybe Value]]
query conn string =
  withBound
    (withHDBC
       conn
       "query"
       (\dbc -> withExecDirect dbc string fetchStatementRows))

--------------------------------------------------------------------------------
-- Internal wrapper functions

-- | Thread-safely access the connection pointer.
withHDBC :: Connection -> String -> (Ptr EnvAndDbc -> IO a) -> IO a
withHDBC conn label f =
  withMVar
    (connectionMVar conn)
    (\mfptr ->
       case mfptr of
         Nothing -> throwIO (DatabaseIsClosed label)
         Just envAndDbc -> do
           v <- withForeignPtr envAndDbc f
           touchForeignPtr envAndDbc
           pure v)

-- | Execute a query directly without preparation.
withExecDirect :: Ptr EnvAndDbc -> Text -> (forall s. SQLHSTMT s -> IO a) -> IO a
withExecDirect dbc string cont =
  withStmt
    dbc
    (\stmt -> do
       assertSuccess
         "odbc_SQLExecDirectW"
         (T.useAsPtr
            string
            (\wstring len ->
               odbc_SQLExecDirectW stmt (coerce wstring) (fromIntegral len)))
       cont stmt)

-- | Run the function with a statement.
withStmt :: Ptr EnvAndDbc -> (forall s. SQLHSTMT s -> IO a) -> IO a
withStmt hdbc =
  bracket
    (assertNotNull "odbc_SQLAllocStmt" (odbc_SQLAllocStmt hdbc))
    odbc_SQLFreeStmt

-- | Run an action in a bound thread. This is neccessary due to the
-- interaction with signals in ODBC and GHC's runtime.
withBound :: MonadIO m => IO a -> m a
withBound = liftIO . flip withAsyncBound wait

--------------------------------------------------------------------------------
-- Internal data retrieval functions

-- | Fetch all rows from a statement.
fetchStatementRows :: SQLHSTMT s -> IO [[Maybe Value]]
fetchStatementRows stmt = do
  SQLSMALLINT cols <-
    withMalloc
      (\sizep -> do
         assertSuccess
           "odbc_SQLNumResultCols"
           (odbc_SQLNumResultCols stmt sizep)
         peek sizep)
  types <- mapM (describeColumn stmt) [1 .. cols]
  let loop rows = do
        do retcode0 <- odbc_SQLFetch stmt
           if | retcode0 == sql_no_data ->
                do retcode <- odbc_SQLMoreResults stmt
                   if retcode == sql_success ||
                      retcode == sql_success_with_info
                     then loop rows
                     else pure (rows [])
              | retcode0 == sql_success ||
                  retcode0 == sql_success_with_info ->
                do fields <- sequence (zipWith (getData stmt) [1 ..] types)
                   loop (rows . (fields :))
              | otherwise ->
                throwIO (UnsuccessfulReturnCode "odbc_SQLFetch" (coerce retcode0))
  loop id

-- | Describe the given column by its integer index.
describeColumn :: SQLHSTMT s -> Int16 -> IO Column
describeColumn stmt i =
  T.useAsPtr
    (T.replicate 1000 "0")
    (\namep namelen ->
       (withMalloc
          (\namelenp ->
             (withMalloc
                (\typep ->
                   withMalloc
                     (\sizep ->
                        withMalloc
                          (\digitsp ->
                             withMalloc
                               (\nullp -> do
                                  assertSuccess
                                    "odbc_SQLDescribeColW"
                                    (odbc_SQLDescribeColW
                                       stmt
                                       (SQLUSMALLINT (fromIntegral i))
                                       (coerce namep)
                                       (SQLSMALLINT (fromIntegral namelen))
                                       namelenp
                                       typep
                                       sizep
                                       digitsp
                                       nullp)
                                  typ <- peek typep
                                  size <- peek sizep
                                  digits <- peek digitsp
                                  isnull <- peek nullp
                                  evaluate
                                    Column
                                    { columnType = typ
                                    , columnSize = size
                                    , columnDigits = digits
                                    , columnNull = isnull
                                    }))))))))

-- | Pull data for the given column.
getData :: SQLHSTMT s -> Int -> Column -> IO (Maybe Value)
getData stmt i col =
  if | colType == sql_longvarchar ->
       let maxChars = coerce (columnSize col) :: Word64
           allocBytes = maxChars + 1
       in do bufferp <- callocBytes (fromIntegral allocBytes)
             mlen <-
               apply
                 "sql_longvarchar/sql_c_char"
                 sql_c_char
                 (coerce bufferp)
                 (SQLLEN (fromIntegral allocBytes))
             case mlen of
               Just len -> do
                 bs <- S.unsafePackMallocCStringLen (bufferp, fromIntegral len)
                 evaluate (Just (BytesValue bs))
               Nothing -> pure Nothing
     | colType == sql_varchar ->
       let maxChars = coerce (columnSize col) :: Word64
           allocBytes = maxChars + 1
       in do bufferp <- callocBytes (fromIntegral allocBytes)
             mlen <-
               apply
                 "sql_varchar/sql_c_char"
                 sql_c_char
                 (coerce bufferp)
                 (SQLLEN (fromIntegral allocBytes))
             case mlen of
               Just len -> do
                 bs <- S.unsafePackMallocCStringLen (bufferp, fromIntegral len)
                 evaluate (Just (BytesValue bs))
               Nothing -> pure Nothing
     | colType == sql_wvarchar ->
       let maxChars = coerce (columnSize col) :: Word64
           allocBytes = maxChars * 2 + 2
       in withCallocBytes
            (fromIntegral allocBytes)
            (\bufferp -> do
               mlen <-
                 apply
                   "sql_wvarchar/sql_c_wchar"
                   sql_c_wchar
                   (coerce bufferp)
                   (SQLLEN (fromIntegral allocBytes))
               case mlen of
                 Nothing -> pure Nothing
                 Just len -> do
                   bs <- S.packCStringLen (bufferp, fromIntegral len)
                   let !v = TextValue (T.decodeUtf16LE bs)
                   pure (Just v))
     | colType == sql_wlongvarchar ->
       let maxChars = coerce (columnSize col) :: Word64
           allocBytes = maxChars * 2 + 2
       in withCallocBytes
            (fromIntegral allocBytes)
            (\bufferp -> do
               mlen <-
                 apply
                   "sql_wlongvarchar/sql_c_wchar"
                   sql_c_wchar
                   (coerce bufferp)
                   (SQLLEN (fromIntegral allocBytes))
               case mlen of
                 Nothing -> pure Nothing
                 Just len -> do
                   bs <- S.packCStringLen (bufferp, fromIntegral len)
                   let !v = TextValue (T.decodeUtf16LE bs)
                   pure (Just v))
     | colType == sql_bit ->
       withMalloc
         (\bitPtr -> do
            mlen <-
              apply "sql_bit/sql_c_bit" sql_c_bit (coerce bitPtr) (SQLLEN 1)
            case mlen of
              Nothing -> pure Nothing
              Just {} ->
                fmap (Just . BoolValue . (/= (0 :: Word8))) (peek bitPtr))
     | colType == sql_double ->
       withMalloc
         (\doublePtr -> do
            mlen <-
              apply
                "sql_double/sql_c_double"
                sql_c_double
                (coerce doublePtr)
                (SQLLEN 8)
            case mlen of
              Nothing -> pure Nothing
              Just {} -> do
                !d <- fmap DoubleValue (peek doublePtr)
                pure (Just d))
     | colType == sql_float ->
       withMalloc
         (\doublePtr -> do
            mlen <-
              apply
                "sql_float/sql_c_double"
                sql_c_double
                (coerce doublePtr)
                (SQLLEN 8)
            case mlen of
              Nothing -> pure Nothing
              Just {} -> do
                !d <- fmap DoubleValue (peek doublePtr)
                pure (Just d))
     | colType == sql_integer ->
       withMalloc
         (\intPtr -> do
            mlen <-
              apply
                "sql_integer/sql_c_long"
                sql_c_long
                (coerce intPtr)
                (SQLLEN 4)
            case mlen of
              Nothing -> pure Nothing
              Just {} ->
                fmap
                  (Just . IntValue . fromIntegral)
                  (peek (intPtr :: Ptr Int32)))
     | colType == sql_smallint ->
       withMalloc
         (\intPtr -> do
            mlen <-
              apply
                "sql_smallint/sql_c_short"
                sql_c_short
                (coerce intPtr)
                (SQLLEN 2)
            case mlen of
              Nothing -> pure Nothing
              Just {} ->
                fmap
                  (Just . IntValue . fromIntegral)
                  (peek (intPtr :: Ptr Int16)))
     | otherwise ->
       throwIO
         (UnknownDataType
            "getData"
            (let SQLSMALLINT n = colType
             in n))
  where
    colType = columnType col
    apply label ty bufferp bufferlen =
      withMalloc
        (\copiedPtr -> do
           assertSuccess
             (label <> "/odbc_SQLGetData")
             (odbc_SQLGetData
                stmt
                (SQLUSMALLINT (fromIntegral i))
                ty
                bufferp
                bufferlen
                copiedPtr)
           copiedBytes <- peek copiedPtr
           if copiedBytes == sql_null_data
             then pure Nothing
             else pure (Just (coerce copiedBytes :: Int64)))

--------------------------------------------------------------------------------
-- Correctness checks

-- | Check that the RETCODE is successful.
assertNotNull :: (Coercible a (Ptr ())) => String -> IO a -> IO a
assertNotNull label m = do
  val <- m
  if coerce val == nullPtr
    then throwIO (AllocationReturnedNull label)
    else pure val

-- | Check that the RETCODE is successful.
assertSuccess :: String -> IO RETCODE -> IO ()
assertSuccess label m = do
  retcode <- m
  if retcode == sql_success || retcode == sql_success_with_info
    then pure ()
    else throwIO (UnsuccessfulReturnCode label (coerce retcode))

--------------------------------------------------------------------------------
-- Foreign types
--
-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h

-- Opaque pointers

-- | An environment and database connection in one go.
data EnvAndDbc

-- | The handle allocated for any query.
newtype SQLHSTMT s = SQLHSTMT (Ptr (SQLHSTMT s))

-- | Used to get data.
newtype SQLPOINTER = SQLPOINTER (Ptr SQLPOINTER)

-- | A type that maps to https://docs.microsoft.com/en-us/sql/odbc/reference/appendixes/c-data-types
newtype SQLCTYPE =
  SQLCTYPE Int16
  deriving (Show, Eq, Storable)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L152
newtype RETCODE = RETCODE Int16
  deriving (Show, Eq)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L89
newtype SQLUSMALLINT = SQLUSMALLINT Word16 deriving (Show, Eq, Storable)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L52..L52
newtype SQLUCHAR = SQLUCHAR Word8 deriving (Show, Eq, Storable)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L52..L52
newtype SQLCHAR = SQLCHAR CChar deriving (Show, Eq, Storable)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L88
newtype SQLSMALLINT = SQLSMALLINT Int16 deriving (Show, Eq, Storable, Num)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L64
newtype SQLLEN = SQLLEN Int64 deriving (Show, Eq, Storable, Num)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L65..L65
newtype SQLULEN = SQLULEN Word64 deriving (Show, Eq, Storable)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L60
newtype SQLINTEGER = SQLINTEGER Int64 deriving (Show, Eq, Storable, Num)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L332
newtype SQLWCHAR = SQLWCHAR CWString deriving (Show, Eq, Storable)

--------------------------------------------------------------------------------
-- Foreign functions

foreign import ccall "odbc odbc_AllocEnvAndDbc"
  odbc_AllocEnvAndDbc :: IO (Ptr EnvAndDbc)

foreign import ccall "odbc &odbc_FreeEnvAndDbc"
  odbc_FreeEnvAndDbc :: FunPtr (Ptr EnvAndDbc -> IO ())

foreign import ccall "odbc odbc_SQLDriverConnectW"
  odbc_SQLDriverConnectW :: Ptr EnvAndDbc -> SQLWCHAR -> SQLSMALLINT -> IO RETCODE

foreign import ccall "odbc &odbc_SQLDisconnect"
  odbc_SQLDisconnect :: FunPtr (Ptr EnvAndDbc -> IO ())

foreign import ccall "odbc odbc_SQLAllocStmt"
  odbc_SQLAllocStmt :: Ptr EnvAndDbc -> IO (SQLHSTMT s)

foreign import ccall "odbc odbc_SQLFreeStmt"
  odbc_SQLFreeStmt :: SQLHSTMT s -> IO ()

foreign import ccall "odbc odbc_SQLExecDirectW"
  odbc_SQLExecDirectW :: SQLHSTMT s -> SQLWCHAR -> SQLINTEGER -> IO RETCODE

foreign import ccall "odbc odbc_SQLFetch"
  odbc_SQLFetch :: SQLHSTMT s -> IO RETCODE

foreign import ccall "odbc odbc_SQLMoreResults"
  odbc_SQLMoreResults :: SQLHSTMT s -> IO RETCODE

foreign import ccall "odbc odbc_SQLNumResultCols"
  odbc_SQLNumResultCols :: SQLHSTMT s -> Ptr SQLSMALLINT -> IO RETCODE

foreign import ccall "odbc odbc_SQLGetData"
 odbc_SQLGetData
  :: SQLHSTMT s
  -> SQLUSMALLINT
  -> SQLCTYPE
  -> SQLPOINTER
  -> SQLLEN
  -> Ptr SQLLEN
  -> IO RETCODE

foreign import ccall "odbc odbc_SQLDescribeColW"
  odbc_SQLDescribeColW
    :: SQLHSTMT s
    -> SQLUSMALLINT
    -> Ptr SQLWCHAR
    -> SQLSMALLINT
    -> Ptr SQLSMALLINT
    -> Ptr SQLSMALLINT
    -> Ptr SQLULEN
    -> Ptr SQLSMALLINT
    -> Ptr SQLSMALLINT
    -> IO RETCODE

--------------------------------------------------------------------------------
-- Foreign utils

withMalloc :: Storable a => (Ptr a -> IO b) -> IO b
withMalloc m = bracket malloc free m

withCallocBytes :: Storable a => Int -> (Ptr a -> IO b) -> IO b
withCallocBytes n m = bracket (callocBytes n) free m

--------------------------------------------------------------------------------
-- SQL constants

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sql.h#L50..L51
sql_success :: RETCODE
sql_success = RETCODE 0

sql_success_with_info :: RETCODE
sql_success_with_info = RETCODE 1

sql_no_data :: RETCODE
sql_no_data = RETCODE 100

sql_null_data :: SQLLEN
sql_null_data = (-1)

--------------------------------------------------------------------------------
-- SQL data type constants

sql_unknown_type :: SQLSMALLINT
sql_unknown_type = 0

sql_char :: SQLSMALLINT
sql_char = 1

sql_numeric :: SQLSMALLINT
sql_numeric = 2

sql_decimal :: SQLSMALLINT
sql_decimal = 3

sql_integer :: SQLSMALLINT
sql_integer = 4

sql_smallint :: SQLSMALLINT
sql_smallint = 5

sql_float :: SQLSMALLINT
sql_float = 6

sql_real :: SQLSMALLINT
sql_real = 7

sql_double :: SQLSMALLINT
sql_double = 8

sql_datetime :: SQLSMALLINT
sql_datetime = 9

sql_varchar :: SQLSMALLINT
sql_varchar = 12

sql_wchar :: SQLSMALLINT
sql_wchar = (-8)

sql_wvarchar :: SQLSMALLINT
sql_wvarchar = (-9)

sql_wlongvarchar :: SQLSMALLINT
sql_wlongvarchar = (-10)

sql_date :: SQLSMALLINT
sql_date = 9

sql_interval :: SQLSMALLINT
sql_interval = 10

sql_time :: SQLSMALLINT
sql_time = 10

sql_timestamp :: SQLSMALLINT
sql_timestamp = 11

sql_longvarchar :: SQLSMALLINT
sql_longvarchar = (-1)

sql_binary :: SQLSMALLINT
sql_binary = (-2)

sql_varbinary :: SQLSMALLINT
sql_varbinary = (-3)

sql_longvarbinary :: SQLSMALLINT
sql_longvarbinary = (-4)

sql_bigint :: SQLSMALLINT
sql_bigint = (-5)

sql_tinyint :: SQLSMALLINT
sql_tinyint = (-6)

sql_bit :: SQLSMALLINT
sql_bit = (-7)

sql_guid :: SQLSMALLINT
sql_guid = (-11)

--------------------------------------------------------------------------------
-- C type constants

sql_c_wchar :: SQLCTYPE
sql_c_wchar = coerce sql_wchar

sql_c_char :: SQLCTYPE
sql_c_char = coerce sql_char

sql_c_double :: SQLCTYPE
sql_c_double = coerce sql_double

sql_c_long :: SQLCTYPE
sql_c_long = coerce sql_integer

sql_c_short :: SQLCTYPE
sql_c_short = coerce sql_smallint

sql_c_bit :: SQLCTYPE
sql_c_bit = coerce sql_bit
