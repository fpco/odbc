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

-- | Trivial bindings to ODBC for testing purposes.

module ODBC
  (
    -- * Connection
    connect
    -- * Executing queries
  , exec
  , query
    -- * Types
  , Value(..)
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception
import qualified Data.ByteString as S
import           Data.Coerce
import           Data.Data
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Foreign as T
import           Foreign
import           Foreign.C
import           GHC.Generics

--------------------------------------------------------------------------------
-- Constants

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sql.h#L50..L51
sql_success :: RETCODE
sql_success = RETCODE 0

sql_success_with_info :: RETCODE
sql_success_with_info = RETCODE 1

sql_no_data :: RETCODE
sql_no_data = RETCODE 100

--------------------------------------------------------------------------------
-- Types

-- | A database exception.
data ODBCException
  = UnsuccessfulReturnCode !String !RETCODE
  | AllocationReturnedNull !String
  | UnknownType !Int16
  | DatabaseIsClosed !String
  deriving (Typeable, Show, Eq)
instance Exception ODBCException

-- | A column description.
data Column = Column
  { columnType :: !SQLSMALLINT
  , columnSize :: !SQLULEN
  , columnDigits :: !SQLSMALLINT
  , columnNull :: !SQLSMALLINT
  } deriving (Show)

-- | A value used for input/output with the database.
data Value
  = TextValue !Text
  | BoolValue !Bool
  | DoubleValue !Double
  | IntValue !Int32
  deriving (Eq, Show, Typeable, Ord, Generic, Data)
instance NFData Value

-- | Connection to a database.
newtype Connection = Connection
  {connectionMVar :: MVar (Maybe ConnectionState)}

-- | Container of unsafe pointers. This value should only be accessed
-- from within the 'Connection' type.
data ConnectionState = ConnectionState
  { connectionEnv :: !(ForeignPtr SQLHENV)
  , connectionDbc  :: !(ForeignPtr SQLHDBC)
  }

--------------------------------------------------------------------------------
-- Exposed functions

-- | Connect using the given connection string.
connect ::
     Text -- ^ Connection string.
  -> IO Connection
connect string =
  withBound
    (do env <-
          uninterruptibleMask_
            (do ptr <- assertNotNull "odbc_SQLAllocEnv" odbc_SQLAllocEnv
                newForeignPtr odbc_SQLFreeEnv (coerce ptr))
        assertSuccess "odbc_SetEnvAttr" (withForeignPtr env odbc_SetEnvAttr)
        -- Above: Allocate the environment.
        -- Below: Allocate a database handle, not connected.
        dbc <-
          uninterruptibleMask_
            (do ptr <-
                  assertNotNull
                    "odbc_SQLAllocDbc"
                    (withForeignPtr env odbc_SQLAllocDbc)
                newForeignPtr odbc_SQLFreeDbc (coerce ptr))
        -- Below: Try to connect to the database.
        T.useAsPtr
          string
          (\wstring len ->
             uninterruptibleMask_
               (do assertSuccess
                     "odbc_SQLDriverConnect"
                     (withForeignPtr
                        dbc
                        (\dbcPtr ->
                           odbc_SQLDriverConnectW
                             dbcPtr
                             (coerce wstring)
                             (fromIntegral len)))
                   addForeignPtrFinalizer odbc_SQLDisconnect dbc))
        -- Below: Keep the environment and the database handle in an mvar.
        mvar <-
          newMVar
            (Just (ConnectionState {connectionEnv = env, connectionDbc = dbc}))
        pure (Connection mvar))

-- | Exec a statement on the database.
exec :: Connection -> Text -> IO ()
exec conn string =
  withBound
    (withHDBC conn "exec" (\dbc -> withExecDirect dbc string (const (pure ()))))

-- | Query and produce a strict list.
query :: Connection -> Text -> IO [[Value]]
query conn string =
  withBound
    (withHDBC
       conn
       "query"
       (\dbc -> withExecDirect dbc string fetchStatementRows))

--------------------------------------------------------------------------------
-- Internal wrapper functions

-- | Thread-safely access the connection pointer.
withHDBC :: Connection -> String -> (Ptr SQLHDBC -> IO a) -> IO a
withHDBC conn label f =
  withMVar
    (connectionMVar conn)
    (\mfptr ->
       case mfptr of
         Nothing -> throwIO (DatabaseIsClosed label)
         Just (ConnectionState {connectionDbc = db,connectionEnv=env}) -> do
           v <- withForeignPtr db f
           touchForeignPtr db
           touchForeignPtr env
           pure v)

-- | Execute a query directly without preparation.
withExecDirect :: Ptr SQLHDBC -> Text -> (forall s. SQLHSTMT s -> IO a) -> IO a
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
withStmt :: Ptr SQLHDBC -> (forall s. SQLHSTMT s -> IO a) -> IO a
withStmt hdbc =
  bracket
    (assertNotNull "odbc_SQLAllocStmt" (odbc_SQLAllocStmt hdbc))
    odbc_SQLFreeStmt

-- | Run an action in a bound thread. This is neccessary due to the
-- interaction with signals in ODBC and GHC's runtime.
withBound :: IO a -> IO a
withBound = flip withAsyncBound wait

--------------------------------------------------------------------------------
-- Internal data retrieval functions

-- | Fetch all rows from a statement.
fetchStatementRows :: SQLHSTMT s -> IO [[Value]]
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
                throwIO (UnsuccessfulReturnCode "odbc_SQLFetch" retcode0)
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
getData :: SQLHSTMT s -> Int -> Column -> IO Value
getData stmt i col =
  case columnType col of
    SQLSMALLINT (-9) {-SQL_WVARCHAR-}
     ->
      withCallocBytes
        (fromIntegral allocBytes)
        (\bufferp -> do
           withMalloc
             (\copiedPtr -> do
                apply
                  (SQLSMALLINT (-8))
                  (coerce bufferp)
                  (SQLLEN (fromIntegral allocBytes))
                  copiedPtr
                SQLLEN copiedBytes <- peek copiedPtr
                bs <- S.packCStringLen (bufferp, fromIntegral copiedBytes)
                evaluate (TextValue (T.decodeUtf16LE bs))))
      where maxChars = coerce (columnSize col) :: Word64
            allocBytes = maxChars * 2 + 2
    SQLSMALLINT (-7) {-SQL_BIT-}
     ->
      withMalloc
        (\ignored ->
           withMalloc
             (\bitPtr -> do
                apply (columnType col) (coerce bitPtr) (SQLLEN 1) ignored
                fmap (BoolValue . (/= (0 :: Word8))) (peek bitPtr)))
    SQLSMALLINT 6 {-SQL_FLOAT-}
     ->
      withMalloc
        (\doublePtr ->
           withMalloc
             (\ignored -> do
                apply (SQLSMALLINT 8) (coerce doublePtr) (SQLLEN 8) ignored
                !d <- fmap DoubleValue (peek doublePtr)
                pure d))
    SQLSMALLINT 4 {-SQL_INTEGER-}
     ->
      withMalloc
        (\intPtr ->
           withMalloc
             (\ignored -> do
                apply (columnType col) (coerce intPtr) (SQLLEN 4) ignored
                fmap IntValue (peek intPtr)))
    SQLSMALLINT 5 {-SQL_INTEGER-}
     ->
      withMalloc
        (\intPtr ->
           withMalloc
             (\ignored -> do
                apply (columnType col) (coerce intPtr) (SQLLEN 2) ignored
                fmap (IntValue . fromIntegral) (peek (intPtr :: Ptr Int16))))
    _ ->
      throwIO
        (UnknownType
           (let SQLSMALLINT n = columnType col
            in n))
  where
    apply ty bufferp bufferlen strlenp =
      assertSuccess
        "odbc_SQLGetData"
        (odbc_SQLGetData
           stmt
           (SQLUSMALLINT (fromIntegral i))
           ty
           bufferp
           bufferlen
           strlenp)

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
    else throwIO (UnsuccessfulReturnCode label retcode)

--------------------------------------------------------------------------------
-- Foreign types
--
-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h

-- Opaque pointers

-- | An environment. Allocated once per connection.
data SQLHENV

-- | A database connection.
data SQLHDBC

-- | The handle allocated for any query.
newtype SQLHSTMT s = SQLHSTMT (Ptr (SQLHSTMT s))

-- | Used to get data.
newtype SQLPOINTER = SQLPOINTER (Ptr SQLPOINTER)

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

foreign import ccall "odbc odbc_SQLAllocEnv"
  odbc_SQLAllocEnv :: IO (Ptr SQLHENV)

foreign import ccall "odbc &odbc_SQLFreeEnv"
  odbc_SQLFreeEnv :: FunPtr (Ptr SQLHENV -> IO ())

foreign import ccall "odbc odbc_SetEnvAttr"
  odbc_SetEnvAttr :: Ptr SQLHENV -> IO RETCODE

foreign import ccall "odbc odbc_SQLAllocDbc"
  odbc_SQLAllocDbc :: Ptr SQLHENV -> IO (Ptr SQLHDBC)

foreign import ccall "odbc &odbc_SQLFreeDbc"
  odbc_SQLFreeDbc :: FunPtr (Ptr SQLHDBC -> IO ())

foreign import ccall "odbc odbc_SQLDriverConnect"
  odbc_SQLDriverConnectW :: Ptr SQLHDBC -> SQLWCHAR -> SQLSMALLINT -> IO RETCODE

foreign import ccall "odbc &odbc_SQLDisconnect"
  odbc_SQLDisconnect :: FunPtr (Ptr SQLHDBC -> IO ())

foreign import ccall "odbc odbc_SQLAllocStmt"
  odbc_SQLAllocStmt :: Ptr SQLHDBC -> IO (SQLHSTMT s)

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
  -> SQLSMALLINT
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
