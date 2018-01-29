{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Trivial bindings to ODBC for testing purposes.

module ODBC
  ( withEnv
  , withDbc
  , queryRows
  , queryDiscarding
  , preparedExec
  , Value(..)
  , SQLHSTMT
  , SQLHDBC
  , SQLHENV
  ) where

import           Control.Concurrent.Async
import           Control.DeepSeq
import           Control.Exception
import qualified Data.ByteString as S
import           Data.Coerce
import           Data.Data
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
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

data ODBCException
  = UnsuccessfulReturnCode String RETCODE
  | AllocationReturnedNull String
  | UnknownType Int16
  deriving (Typeable, Show)
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

--------------------------------------------------------------------------------
-- Exposed functions

preparedExec :: SQLHDBC t -> String -> IO [[Value]]
preparedExec dbc string =
  withPrepare
    dbc
    string
    (\stmt -> do
       assertSuccess "odbc_SQLExec" (odbc_SQLExec stmt)
       fetchStatementRows stmt)

-- | Query, process and discard all rows.
queryDiscarding :: SQLHDBC t -> String -> IO Int
queryDiscarding dbc string =
  withExecDirect
    dbc
    string
    (\stmt -> do
       let loop !rows = do
             do retcode0 <- odbc_SQLFetch stmt
                if | retcode0 == sql_no_data ->
                     do retcode <- odbc_SQLMoreResults stmt
                        if retcode == sql_success ||
                           retcode == sql_success_with_info
                          then loop rows
                          else pure rows
                   | retcode0 == sql_success ||
                       retcode0 == sql_success_with_info -> loop (rows + 1)
                   | otherwise ->
                     throwIO (UnsuccessfulReturnCode "odbc_SQLFetch" retcode0)
       loop 0)

-- | Query and return all rows.
queryRows :: SQLHDBC t -> String -> IO [[Value]]
queryRows dbc string = withExecDirect dbc string fetchStatementRows

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

describeColumn :: Integral a => SQLHSTMT s -> a -> IO Column
describeColumn stmt i =
  withCStringLen
    (replicate 100 '\0')
    (\(namep, namelen) ->
       withMalloc
         (\namelenp ->
            withMalloc
              (\typep ->
                 withMalloc
                   (\sizep ->
                      withMalloc
                        (\digitsp ->
                           withMalloc
                             (\nullp -> do
                                assertSuccess
                                  "odbc_SQLDescribeCol"
                                  (odbc_SQLDescribeCol
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
                                  }))))))


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

withPrepare :: SQLHDBC t -> String -> (forall s. SQLHSTMT s -> IO a) -> IO a
withPrepare dbc string cont =
  withStmt
    dbc
    (\stmt -> do
       assertSuccess
         "odbc_SQLPrepare"
         (withCString string (odbc_SQLPrepare stmt))
       cont stmt)

withExecDirect :: SQLHDBC t -> String -> (forall s. SQLHSTMT s -> IO a) -> IO a
withExecDirect dbc string cont =
  withStmt
    dbc
    (\stmt -> do
       assertSuccess
         "odbc_SQLExecDirect"
         (withCString string (odbc_SQLExecDirect stmt))
       cont stmt)

-- | Run the function with an environment.
withEnv :: (forall s. SQLHENV s -> IO a) -> IO a
withEnv cont =
  bracket
    (assertNotNull "odbc_SQLAllocEnv" odbc_SQLAllocEnv)
    odbc_SQLFreeEnv
    (\env -> do
       assertSuccess "odbc_SetEnvAttr" (odbc_SetEnvAttr env)
       cont env)

-- | Run the function with a database handle.
withDbc :: SQLHENV t -> String -> (forall s. SQLHDBC s -> IO a) -> IO a
withDbc henv connString cont =
  bracket
    (assertNotNull "odbc_SQLAllocDbc" (odbc_SQLAllocDbc henv))
    odbc_SQLFreeDbc
    (\dbc ->
       withBound
         (do bracket
               (withCString
                  connString
                  (assertSuccess "odbc_SQLDriverConnect" .
                   odbc_SQLDriverConnect dbc))
               (const (odbc_SQLDisconnect dbc))
               (const (cont dbc))))

-- | Run the function with a statement.
withStmt :: SQLHDBC t -> (forall s. SQLHSTMT s -> IO a) -> IO a
withStmt hdbc =
  bracket
    (assertNotNull "odbc_SQLAllocStmt" (odbc_SQLAllocStmt hdbc))
    odbc_SQLFreeStmt

-- | Run an action in a bound thread.
withBound :: IO a -> IO a
withBound = flip withAsyncBound wait

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

newtype SQLHENV s = SQLHENV (Ptr (SQLHENV s))
newtype SQLHDBC s = SQLHDBC (Ptr (SQLHDBC s))
newtype SQLHSTMT s = SQLHSTMT (Ptr (SQLHSTMT s))
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
newtype SQLSMALLINT = SQLSMALLINT Int16 deriving (Show, Eq, Storable)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L64
newtype SQLLEN = SQLLEN Int64 deriving (Show, Eq, Storable)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L65..L65
newtype SQLULEN = SQLULEN Word64 deriving (Show, Eq, Storable)

--------------------------------------------------------------------------------
-- Foreign functions

foreign import ccall "odbc odbc_SQLAllocEnv"
  odbc_SQLAllocEnv :: IO (SQLHENV s)

foreign import ccall "odbc odbc_SQLFreeEnv"
  odbc_SQLFreeEnv :: SQLHENV s -> IO ()

foreign import ccall "odbc odbc_SetEnvAttr"
  odbc_SetEnvAttr :: SQLHENV s -> IO RETCODE

foreign import ccall "odbc odbc_SQLAllocDbc"
  odbc_SQLAllocDbc :: SQLHENV s -> IO (SQLHDBC s)

foreign import ccall "odbc odbc_SQLFreeDbc"
  odbc_SQLFreeDbc :: SQLHDBC s -> IO ()

foreign import ccall "odbc odbc_SQLDriverConnect"
  odbc_SQLDriverConnect :: SQLHDBC s -> CString -> IO RETCODE

foreign import ccall "odbc odbc_SQLDisconnect"
  odbc_SQLDisconnect :: SQLHDBC s -> IO ()

foreign import ccall "odbc odbc_SQLAllocStmt"
  odbc_SQLAllocStmt :: SQLHDBC s -> IO (SQLHSTMT s)

foreign import ccall "odbc odbc_SQLFreeStmt"
  odbc_SQLFreeStmt :: SQLHSTMT s -> IO ()

foreign import ccall "odbc odbc_SQLExecDirect"
  odbc_SQLExecDirect :: SQLHSTMT s -> CString -> IO RETCODE

foreign import ccall "odbc odbc_SQLExec"
  odbc_SQLExec :: SQLHSTMT s -> IO RETCODE

foreign import ccall "odbc odbc_SQLPrepare"
  odbc_SQLPrepare :: SQLHSTMT s -> CString -> IO RETCODE

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

foreign import ccall "odbc odbc_SQLDescribeCol"
  odbc_SQLDescribeCol
    :: SQLHSTMT s
    -> SQLUSMALLINT
    -> Ptr SQLCHAR
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
