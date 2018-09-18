{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
--
-- WARNING: This API is meant as a base for more high-level APIs, such
-- as the one provided in "Database.ODBC.SQLServer". The commands here
-- are all vulerable to SQL injection attacks. See
-- <https://en.wikipedia.org/wiki/SQL_injection> for more information.
--
-- Don't use this module if you don't know what you're doing.

module Database.ODBC.Internal
  ( -- * Connect/disconnect
    connect
  , close
  , withConnection
  , Connection
    -- * Executing queries
  , exec
  , query
  , Value(..)
  , Binary(..)
    -- * Streaming results
  , stream
  , Step(..)
    -- * Exceptions
  , ODBCException(..)
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as S
import           Data.Coerce
import           Data.Data
import           Data.Int
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import           Data.Time
import           Foreign hiding (void)
import           Foreign.C
import           GHC.Generics

--------------------------------------------------------------------------------
-- Public types

-- | Connection to a database. Use of this connection is
-- thread-safe. When garbage collected, the connection will be closed
-- if not done already.
newtype Connection = Connection
  {connectionMVar :: MVar (Maybe (ForeignPtr EnvAndDbc))}

-- | A database exception. Any of the functions in this library may
-- throw this exception type.
data ODBCException
  = UnsuccessfulReturnCode !String
                           !Int16
                           !String
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
  | NoTotalInformation !Int
    -- ^ No total length information for column.
  | DataRetrievalError !String
    -- ^ There was a general error retrieving data. String will
    -- contain the reason why.
  deriving (Typeable, Show, Eq)
instance Exception ODBCException

-- | A value used for input/output with the database.
data Value
  = TextValue !Text
    -- ^ A Unicode text value.
  | ByteStringValue !ByteString
    -- ^ A vector of bytes. It might be binary, or a string, but we
    -- don't know the encoding. Use 'Data.Text.Encoding.decodeUtf8' if
    -- the string is UTF-8 encoded, or
    -- 'Data.Text.Encoding.decodeUtf16LE' if it is UTF-16 encoded. For
    -- other encodings, see the Haskell text-icu package.  For raw
    -- binary, see 'BinaryValue'.
  | BinaryValue !Binary
    -- ^ Only a vector of bytes. Intended for binary data, not for
    -- ASCII text.
  | BoolValue !Bool
    -- ^ A simple boolean.
  | DoubleValue !Double
    -- ^ Floating point values that fit in a 'Double'.
  | FloatValue !Float
    -- ^ Floating point values that fit in a 'Float'.
  | IntValue !Int
    -- ^ Integer values that fit in an 'Int'.
  | ByteValue !Word8
    -- ^ Values that fit in one byte.
  | DayValue !Day
    -- ^ Date (year, month, day) values.
  | TimeOfDayValue !TimeOfDay
    -- ^ Time of day (hh, mm, ss + fractional) values.
  | LocalTimeValue !LocalTime
    -- ^ Local date and time.
  | NullValue
    -- ^ SQL null value.
  deriving (Eq, Show, Typeable, Ord, Generic, Data)
instance NFData Value

-- | A simple newtype wrapper around the 'ByteString' type to use when
-- you want to mean the @binary@ type of SQL, and render to binary
-- literals e.g. @0xFFEF01@.
--
-- The 'ByteString' type is already mapped to the non-Unicode 'text'
-- type.
newtype Binary = Binary
  { unBinary :: ByteString
  } deriving (Show, Eq, Ord, Data, Generic, Typeable, NFData)

-- | A step in the streaming process for the 'stream' function.
data Step a
  = Stop !a     -- ^ Stop with this value.
  | Continue !a -- ^ Continue with this value.
  deriving (Show)

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
  => Text -- ^ An ODBC connection string.
  -> m Connection
  -- ^ A connection to the database. You should call 'close' on it
  -- when you're done. If you forget to, then the connection will only
  -- be closed when there are no more references to the 'Connection'
  -- value in your program, which might never happen. So take care.
  -- Use e.g. 'bracket' from "Control.Exception" to do the open/close
  -- pattern, which will handle exceptions.
connect string =
  withBound
    (do (ptr, envAndDbc) <-
          uninterruptibleMask_
            (do ptr <- assertNotNull "odbc_AllocEnvAndDbc" odbc_AllocEnvAndDbc
                fmap (ptr, ) (newForeignPtr odbc_FreeEnvAndDbc (coerce ptr)))
             -- Above: Allocate the environment.
             -- Below: Try to connect to the database.
        withCStringLen
          (T.unpack string)
          (\(wstring,len) ->
             uninterruptibleMask_
               (do assertSuccess
                     ptr
                     "odbc_SQLDriverConnect"
                     (withForeignPtr
                        envAndDbc
                        (\dbcPtr ->
                           odbc_SQLDriverConnect
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
close ::
     MonadIO m
  => Connection -- ^ A connection to the database.
  -> m ()
close conn =
  withBound
    (do mstate <- modifyMVar (connectionMVar conn) (pure . (Nothing, ))
        -- If an async exception comes after here, that's a pity
        -- because we wanted to free the connection now. But with
        -- regards to safety, the finalizers will take care of closing
        -- the connection and the env.
        maybe (throwIO DatabaseAlreadyClosed) finalizeForeignPtr mstate)


-- | Memory bracket around 'connect' and 'close'.
withConnection :: MonadUnliftIO m =>
               Text  -- ^ An ODBC connection string.
            -> (Connection -> m a) -- ^ Program that uses the ODBC connection.
            -> m a
withConnection str inner = withRunInIO $ \io ->
  withBound $ bracket (connect str) close (\h -> io (inner h))


-- | Execute a statement on the database.
exec ::
     MonadIO m
  => Connection -- ^ A connection to the database.
  -> Text -- ^ SQL statement.
  -> m ()
exec conn string =
  withBound
    (withHDBC
       conn
       "exec"
       (\dbc -> withExecDirect dbc string (fetchAllResults dbc)))

-- | Query and return a list of rows.
query ::
     MonadIO m
  => Connection -- ^ A connection to the database.
  -> Text -- ^ SQL query.
  -> m [[Value]]
  -- ^ A strict list of rows. This list is not lazy, so if you are
  -- retrieving a large data set, be aware that all of it will be
  -- loaded into memory.
query conn string =
  withBound
    (withHDBC
       conn
       "query"
       (\dbc -> withExecDirect dbc string (fetchStatementRows dbc)))

-- | Stream results like a fold with the option to stop at any time.
stream ::
     (MonadIO m, MonadUnliftIO m)
  => Connection -- ^ A connection to the database.
  -> Text -- ^ SQL query.
  -> (state -> [Value] -> m (Step state))
  -- ^ A stepping function that gets as input the current @state@ and
  -- a row, returning either a new @state@ or a final @result@.
  -> state
  -- ^ A state that you can use for the computation. Strictly
  -- evaluated each iteration.
  -> m state
  -- ^ Final result, produced by the stepper function.
stream conn string step state = do
  unlift <- askUnliftIO
  withBound
    (withHDBC
       conn
       "stream"
       (\dbc ->
          withExecDirect
            dbc
            string
            (fetchIterator dbc unlift step state)))

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
       void
         (assertSuccessOrNoData
            dbc
            "odbc_SQLExecDirectW"
            (T.useAsPtr
               string
               (\wstring len ->
                  odbc_SQLExecDirectW
                    dbc
                    stmt
                    (coerce wstring)
                    (fromIntegral len))))
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

-- | Iterate over the rows in the statement.
fetchIterator ::
     Ptr EnvAndDbc
  -> UnliftIO m
  -> (state -> [Value] -> m (Step state))
  -> state
  -> SQLHSTMT s
  -> IO state
fetchIterator dbc (UnliftIO runInIO) step state0 stmt = do
  SQLSMALLINT cols <-
    liftIO
      (withMalloc
         (\sizep -> do
            assertSuccess
              dbc
              "odbc_SQLNumResultCols"
              (odbc_SQLNumResultCols stmt sizep)
            peek sizep))
  types <- mapM (describeColumn dbc stmt) [1 .. cols]
  let loop state = do
        do retcode0 <- odbc_SQLFetch dbc stmt
           if | retcode0 == sql_no_data ->
                do retcode <- odbc_SQLMoreResults dbc stmt
                   if retcode == sql_success || retcode == sql_success_with_info
                     then loop state
                     else pure state
              | retcode0 == sql_success || retcode0 == sql_success_with_info ->
                do row <-
                     sequence
                       (zipWith (getData dbc stmt) [SQLUSMALLINT 1 ..] types)
                   !state' <- runInIO (step state row)
                   case state' of
                     Stop state'' -> pure state''
                     Continue state'' -> loop state''
              | otherwise ->
                throwIO
                  (UnsuccessfulReturnCode
                     "odbc_SQLFetch"
                     (coerce retcode0)
                     "Unexpected return code")
  if cols > 0
    then loop state0
    else pure state0

-- | Fetch all results from possible multiple statements.
fetchAllResults :: Ptr EnvAndDbc -> SQLHSTMT s -> IO ()
fetchAllResults dbc stmt = do
  retcode <-
    assertSuccessOrNoData
      dbc
      "odbc_SQLMoreResults"
      (odbc_SQLMoreResults dbc stmt)
  when
    (retcode == sql_success || retcode == sql_success_with_info)
    (fetchAllResults dbc stmt)

-- | Fetch all rows from a statement.
fetchStatementRows :: Ptr EnvAndDbc -> SQLHSTMT s -> IO [[Value]]
fetchStatementRows dbc stmt = do
  SQLSMALLINT cols <-
    withMalloc
      (\sizep -> do
         assertSuccess
           dbc
           "odbc_SQLNumResultCols"
           (odbc_SQLNumResultCols stmt sizep)
         peek sizep)
  types <- mapM (describeColumn dbc stmt) [1 .. cols]
  let loop rows = do
        do retcode0 <- odbc_SQLFetch dbc stmt
           if | retcode0 == sql_no_data ->
                do retcode <- odbc_SQLMoreResults dbc stmt
                   if retcode == sql_success || retcode == sql_success_with_info
                     then loop rows
                     else pure (rows [])
              | retcode0 == sql_success || retcode0 == sql_success_with_info ->
                do fields <-
                     sequence
                       (zipWith (getData dbc stmt) [SQLUSMALLINT 1 ..] types)
                   loop (rows . (fields :))
              | otherwise ->
                throwIO
                  (UnsuccessfulReturnCode
                     "odbc_SQLFetch"
                     (coerce retcode0)
                     "Unexpected return code")
  if cols > 0
    then loop id
    else pure []

-- | Describe the given column by its integer index.
describeColumn :: Ptr EnvAndDbc -> SQLHSTMT s -> Int16 -> IO Column
describeColumn dbPtr stmt i =
  T.useAsPtr
    (T.replicate 1000 (fromString "0"))
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
                                    dbPtr
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
getData :: Ptr EnvAndDbc -> SQLHSTMT s -> SQLUSMALLINT -> Column -> IO Value
getData dbc stmt i col =
  if | colType == sql_longvarchar -> getBytesData dbc stmt i
     | colType == sql_varchar -> getBytesData dbc stmt i
     | colType == sql_char -> getBytesData dbc stmt i
     | colType == sql_wvarchar -> getTextData dbc stmt i
     | colType == sql_wchar -> getTextData dbc stmt i
     | colType == sql_wlongvarchar -> getTextData dbc stmt i
     | colType == sql_binary -> getBinaryData dbc stmt i
     | colType == sql_varbinary -> getBinaryData dbc stmt i
     | colType == sql_bit ->
       withMalloc
         (\bitPtr -> do
            mlen <- getTypedData dbc stmt sql_c_bit i (coerce bitPtr) (SQLLEN 1)
            case mlen of
              Nothing -> pure NullValue
              Just {} ->
                fmap (BoolValue . (/= (0 :: Word8))) (peek bitPtr))
     | colType == sql_double ->
       withMalloc
         (\doublePtr -> do
            mlen <-
              getTypedData dbc stmt sql_c_double i (coerce doublePtr) (SQLLEN 8)
            -- float is 8 bytes: https://technet.microsoft.com/en-us/library/ms172424(v=sql.110).aspx
            case mlen of
              Nothing -> pure NullValue
              Just {} -> do
                !d <- fmap DoubleValue (peek doublePtr)
                pure d)
     | colType == sql_float ->
       withMalloc
         (\floatPtr -> do
            mlen <-
              getTypedData dbc stmt sql_c_double i (coerce floatPtr) (SQLLEN 8)
            -- SQLFLOAT is covered by SQL_C_DOUBLE: https://docs.microsoft.com/en-us/sql/odbc/reference/appendixes/c-data-types
            -- Float is 8 bytes: https://technet.microsoft.com/en-us/library/ms172424(v=sql.110).aspx
            case mlen of
              Nothing -> pure NullValue
              Just {} -> do
                !d <- fmap DoubleValue (peek floatPtr)
                pure d)
     | colType == sql_real ->
       withMalloc
         (\floatPtr -> do
            mlen <-
              getTypedData dbc stmt sql_c_double i (coerce floatPtr) (SQLLEN 8)
             -- SQLFLOAT is covered by SQL_C_DOUBLE: https://docs.microsoft.com/en-us/sql/odbc/reference/appendixes/c-data-types
             -- Float is 8 bytes: https://technet.microsoft.com/en-us/library/ms172424(v=sql.110).aspx
            case mlen of
              Nothing -> pure NullValue
              Just {} -> do
                !d <-
                  fmap
                    (FloatValue . (realToFrac :: Double -> Float))
                    (peek floatPtr)
                pure d)
     | colType == sql_numeric || colType == sql_decimal ->
       withMalloc
         (\floatPtr -> do
            mlen <-
              getTypedData dbc stmt sql_c_double i (coerce floatPtr) (SQLLEN 8)
            -- NUMERIC/DECIMAL can be read as FLOAT
            -- Float is 8 bytes: https://technet.microsoft.com/en-us/library/ms172424(v=sql.110).aspx
            case mlen of
              Nothing -> pure NullValue
              Just {} -> do
                !d <- fmap DoubleValue (peek floatPtr)
                pure d)
     | colType == sql_integer ->
       withMalloc
         (\intPtr -> do
            mlen <-
              getTypedData dbc stmt sql_c_long i (coerce intPtr) (SQLLEN 4)
            case mlen of
              Nothing -> pure NullValue
              Just {} ->
                fmap
                  (IntValue . fromIntegral)
                  (peek (intPtr :: Ptr Int32)))
     | colType == sql_bigint ->
       withMalloc
         (\intPtr -> do
            mlen <-
              getTypedData dbc stmt sql_c_bigint i (coerce intPtr) (SQLLEN 8)
            case mlen of
              Nothing -> pure NullValue
              Just {} ->
                fmap
                  (IntValue . fromIntegral)
                  (peek (intPtr :: Ptr Int64)))
     | colType == sql_smallint ->
       withMalloc
         (\intPtr -> do
            mlen <-
              getTypedData dbc stmt sql_c_short i (coerce intPtr) (SQLLEN 2)
            case mlen of
              Nothing -> pure NullValue
              Just {} ->
                fmap
                  (IntValue . fromIntegral)
                  (peek (intPtr :: Ptr Int16)))
     | colType == sql_tinyint ->
       withMalloc
         (\intPtr -> do
            mlen <-
              getTypedData dbc stmt sql_c_short i (coerce intPtr) (SQLLEN 1)
            case mlen of
              Nothing -> pure NullValue
              Just {} -> fmap ByteValue (peek (intPtr :: Ptr Word8)))
     | colType == sql_type_date ->
       withMallocBytes
         3
         (\datePtr -> do
            mlen <-
              getTypedData dbc stmt sql_c_date i (coerce datePtr) (SQLLEN 3)
            case mlen of
              Nothing -> pure NullValue
              Just {} ->
                fmap
                  DayValue
                  (fromGregorian <$>
                   (fmap fromIntegral (odbc_DATE_STRUCT_year datePtr)) <*>
                   (fmap fromIntegral (odbc_DATE_STRUCT_month datePtr)) <*>
                   (fmap fromIntegral (odbc_DATE_STRUCT_day datePtr))))
     | colType == sql_ss_time2 ->
       withCallocBytes
         12 -- This struct is padded to 12 bytes on both 32-bit and 64-bit operating systems.
            -- https://docs.microsoft.com/en-us/sql/relational-databases/native-client-odbc-date-time/data-type-support-for-odbc-date-and-time-improvements
         (\datePtr -> do
            mlen <-
              getTypedData
                dbc
                stmt
                sql_c_time
                i
                (coerce datePtr)
                (SQLLEN 12)
            case mlen of
              Nothing -> pure NullValue
              Just {} ->
                fmap
                  TimeOfDayValue
                  (TimeOfDay <$>
                   (fmap fromIntegral (odbc_TIME_STRUCT_hour datePtr)) <*>
                   (fmap fromIntegral (odbc_TIME_STRUCT_minute datePtr)) <*>
                   (fmap fromIntegral (odbc_TIME_STRUCT_second datePtr))))
     | colType == sql_type_timestamp ->
       withMallocBytes
         16
         (\timestampPtr -> do
            mlen <-
              getTypedData
                dbc
                stmt
                sql_c_type_timestamp
                i
                (coerce timestampPtr)
                (SQLLEN 16)
            case mlen of
              Nothing -> pure NullValue
              Just {} ->
                fmap
                  LocalTimeValue
                  (LocalTime <$>
                   (fromGregorian <$>
                    (fmap fromIntegral (odbc_TIMESTAMP_STRUCT_year timestampPtr)) <*>
                    (fmap
                       fromIntegral
                       (odbc_TIMESTAMP_STRUCT_month timestampPtr)) <*>
                    (fmap fromIntegral (odbc_TIMESTAMP_STRUCT_day timestampPtr))) <*>
                   (TimeOfDay <$>
                    (fmap fromIntegral (odbc_TIMESTAMP_STRUCT_hour timestampPtr)) <*>
                    (fmap
                       fromIntegral
                       (odbc_TIMESTAMP_STRUCT_minute timestampPtr)) <*>
                    ((+) <$>
                     (fmap
                        fromIntegral
                        (odbc_TIMESTAMP_STRUCT_second timestampPtr)) <*>
                     (fmap
                        (\frac -> fromIntegral frac / 1000000000)
                        (odbc_TIMESTAMP_STRUCT_fraction timestampPtr))))))
     | colType == sql_guid -> getGuid dbc stmt i
     | otherwise ->
       throwIO
         (UnknownDataType
            "getData"
            (let SQLSMALLINT n = colType
             in n))
  where
    colType = columnType col

-- | Get a GUID as a binary value.
getGuid :: Ptr EnvAndDbc -> SQLHSTMT s -> SQLUSMALLINT -> IO Value
getGuid dbc stmt column = do
  uninterruptibleMask_
    (do bufferp <- callocBytes odbcGuidBytes
        void
          (getTypedData
             dbc
             stmt
             sql_c_binary
             column
             (coerce bufferp)
             (SQLLEN odbcGuidBytes))
        !bs <- S.unsafePackMallocCStringLen (bufferp, odbcGuidBytes)
        evaluate (BinaryValue (Binary bs)))

-- | Get the column's data as a vector of CHAR.
getBytesData :: Ptr EnvAndDbc -> SQLHSTMT s -> SQLUSMALLINT -> IO Value
getBytesData dbc stmt column = do
  mavailableBytes <- getSize dbc stmt sql_c_binary column
  case mavailableBytes of
    Just 0 -> pure (ByteStringValue mempty)
    Just availableBytes ->
      uninterruptibleMask_
        (do let allocBytes = availableBytes + 1
            bufferp <- callocBytes (fromIntegral allocBytes)
            void
              (getTypedData
                 dbc
                 stmt
                 sql_c_binary
                 column
                 (coerce bufferp)
                 (SQLLEN (fromIntegral allocBytes)))
            bs <-
              S.unsafePackMallocCStringLen
                (bufferp, fromIntegral availableBytes)
            evaluate (ByteStringValue bs))
    Nothing -> pure NullValue

-- | Get the column's data as raw binary.
getBinaryData :: Ptr EnvAndDbc -> SQLHSTMT s -> SQLUSMALLINT -> IO Value
getBinaryData dbc stmt column = do
  mavailableBinary <- getSize dbc stmt sql_c_binary column
  case mavailableBinary of
    Just 0 -> pure (BinaryValue (Binary mempty))
    Just availableBinary ->
      uninterruptibleMask_
        (do let allocBinary = availableBinary
            bufferp <- callocBytes (fromIntegral allocBinary)
            void
              (getTypedData
                 dbc
                 stmt
                 sql_c_binary
                 column
                 (coerce bufferp)
                 (SQLLEN (fromIntegral allocBinary)))
            bs <-
              S.unsafePackMallocCStringLen
                (bufferp, fromIntegral availableBinary)
            evaluate (BinaryValue (Binary bs)))
    Nothing -> pure NullValue

-- | Get the column's data as a text string.
getTextData :: Ptr EnvAndDbc -> SQLHSTMT s -> SQLUSMALLINT -> IO Value
getTextData dbc stmt column = do
  mavailableChars <- getSize dbc stmt sql_c_wchar column
  case mavailableChars of
    Just 0 -> pure (TextValue mempty)
    Nothing -> pure NullValue
    Just availableBytes -> do
      let allocBytes = availableBytes + 2
      withMallocBytes
        (fromIntegral allocBytes)
        (\bufferp -> do
           void
             (getTypedData
                dbc
                stmt
                sql_c_wchar
                column
                (coerce bufferp)
                (SQLLEN (fromIntegral allocBytes)))
           t <- T.fromPtr bufferp (fromIntegral (div availableBytes 2))
           let !v = TextValue t
           pure v)

-- | Get some data into the given pointer.
getTypedData ::
     Ptr EnvAndDbc
  -> SQLHSTMT s
  -> SQLCTYPE
  -> SQLUSMALLINT
  -> SQLPOINTER
  -> SQLLEN
  -> IO (Maybe Int64)
getTypedData dbc stmt ty column bufferp bufferlen =
  withMalloc
    (\copiedPtr -> do
       assertSuccess
         dbc
         ("getTypedData ty=" ++ show ty)
         (odbc_SQLGetData dbc stmt column ty bufferp bufferlen copiedPtr)
       copiedBytes <- peek copiedPtr
       if copiedBytes == sql_null_data
         then pure Nothing
         else pure (Just (coerce copiedBytes :: Int64)))

-- | Get only the size of the data, no copying.
getSize :: Ptr EnvAndDbc -> SQLHSTMT s -> SQLCTYPE -> SQLUSMALLINT -> IO (Maybe Int64)
getSize dbc stmt ty column =
  withMalloc
    (\availablePtr -> do
       withMalloc
         (\bufferp ->
            assertSuccess
              dbc
              "getSize"
              (odbc_SQLGetData
                 dbc
                 stmt
                 column
                 ty
                 (coerce (bufferp :: Ptr CChar))
                 0
                 availablePtr))
       availableBytes <- peek availablePtr
       if availableBytes == sql_null_data
         then pure Nothing
         else if availableBytes == sql_no_total
                then throwIO
                       (NoTotalInformation
                          (let SQLUSMALLINT i = column
                           in fromIntegral i))
                else pure (Just (coerce availableBytes :: Int64)))

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
assertSuccess :: Ptr EnvAndDbc -> String -> IO RETCODE -> IO ()
assertSuccess dbc label m = do
  retcode <- m
  if retcode == sql_success || retcode == sql_success_with_info
    then pure ()
    else do
      ptr <- odbc_error dbc
      string <-
        if nullPtr == ptr
          then pure ""
          else peekCString ptr
      throwIO (UnsuccessfulReturnCode label (coerce retcode) string)

-- | Check that the RETCODE is successful or no data.
assertSuccessOrNoData :: Ptr EnvAndDbc -> String -> IO RETCODE -> IO RETCODE
assertSuccessOrNoData dbc label m = do
  retcode <- m
  if retcode == sql_success ||
     retcode == sql_success_with_info || retcode == sql_no_data
    then pure retcode
    else do
      ptr <- odbc_error dbc
      string <-
        if nullPtr == ptr
          then pure ""
          else peekCString ptr
      throwIO (UnsuccessfulReturnCode label (coerce retcode) string)

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
  deriving (Show, Eq, Storable, Integral, Enum, Real, Num, Ord)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L152
newtype RETCODE = RETCODE Int16
  deriving (Show, Eq)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L89
newtype SQLUSMALLINT = SQLUSMALLINT Word16 deriving (Show, Eq, Storable, Integral, Enum, Real, Num, Ord)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L52..L52
newtype SQLUCHAR = SQLUCHAR Word8 deriving (Show, Eq, Storable)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L52..L52
newtype SQLCHAR = SQLCHAR CChar deriving (Show, Eq, Storable)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L88
newtype SQLSMALLINT = SQLSMALLINT Int16 deriving (Show, Eq, Storable, Num, Integral, Enum, Ord, Real)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L64
newtype SQLLEN = SQLLEN Int64 deriving (Show, Eq, Storable, Num)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L65..L65
newtype SQLULEN = SQLULEN Word64 deriving (Show, Eq, Storable)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L60
newtype SQLINTEGER = SQLINTEGER Int64 deriving (Show, Eq, Storable, Num)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L61..L61
newtype SQLUINTEGER = SQLUINTEGER Word64 deriving (Show, Eq, Storable, Num, Integral, Enum, Ord, Real)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqltypes.h#L332
newtype SQLWCHAR = SQLWCHAR CWString deriving (Show, Eq, Storable)

-- https://docs.microsoft.com/en-us/sql/odbc/reference/appendixes/c-data-types
data DATE_STRUCT

-- https://docs.microsoft.com/en-us/sql/odbc/reference/appendixes/c-data-types
data TIME_STRUCT

-- https://docs.microsoft.com/en-us/sql/odbc/reference/appendixes/c-data-types
data TIMESTAMP_STRUCT

--------------------------------------------------------------------------------
-- Foreign functions

foreign import ccall "odbc odbc_error"
  odbc_error :: Ptr EnvAndDbc -> IO (Ptr CChar)

foreign import ccall "odbc odbc_AllocEnvAndDbc"
  odbc_AllocEnvAndDbc :: IO (Ptr EnvAndDbc)

foreign import ccall "odbc &odbc_FreeEnvAndDbc"
  odbc_FreeEnvAndDbc :: FunPtr (Ptr EnvAndDbc -> IO ())

foreign import ccall "odbc odbc_SQLDriverConnect"
  odbc_SQLDriverConnect :: Ptr EnvAndDbc -> Ptr SQLCHAR -> SQLSMALLINT -> IO RETCODE

foreign import ccall "odbc &odbc_SQLDisconnect"
  odbc_SQLDisconnect :: FunPtr (Ptr EnvAndDbc -> IO ())

foreign import ccall "odbc odbc_SQLAllocStmt"
  odbc_SQLAllocStmt :: Ptr EnvAndDbc -> IO (SQLHSTMT s)

foreign import ccall "odbc odbc_SQLFreeStmt"
  odbc_SQLFreeStmt :: SQLHSTMT s -> IO ()

foreign import ccall "odbc odbc_SQLExecDirectW"
  odbc_SQLExecDirectW :: Ptr EnvAndDbc -> SQLHSTMT s -> SQLWCHAR -> SQLINTEGER -> IO RETCODE

foreign import ccall "odbc odbc_SQLFetch"
  odbc_SQLFetch :: Ptr EnvAndDbc -> SQLHSTMT s -> IO RETCODE

foreign import ccall "odbc odbc_SQLMoreResults"
  odbc_SQLMoreResults :: Ptr EnvAndDbc -> SQLHSTMT s -> IO RETCODE

foreign import ccall "odbc odbc_SQLNumResultCols"
  odbc_SQLNumResultCols :: SQLHSTMT s -> Ptr SQLSMALLINT -> IO RETCODE

foreign import ccall "odbc odbc_SQLGetData"
 odbc_SQLGetData
  :: Ptr EnvAndDbc
  -> SQLHSTMT s
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

foreign import ccall "odbc DATE_STRUCT_year" odbc_DATE_STRUCT_year
 :: Ptr DATE_STRUCT -> IO SQLSMALLINT

foreign import ccall "odbc DATE_STRUCT_month" odbc_DATE_STRUCT_month
 :: Ptr DATE_STRUCT -> IO SQLUSMALLINT

foreign import ccall "odbc DATE_STRUCT_day" odbc_DATE_STRUCT_day
 :: Ptr DATE_STRUCT -> IO SQLUSMALLINT

foreign import ccall "odbc TIME_STRUCT_hour" odbc_TIME_STRUCT_hour
  :: Ptr TIME_STRUCT -> IO SQLUSMALLINT
foreign import ccall "odbc TIME_STRUCT_minute" odbc_TIME_STRUCT_minute
  :: Ptr TIME_STRUCT -> IO SQLUSMALLINT
foreign import ccall "odbc TIME_STRUCT_second" odbc_TIME_STRUCT_second
  :: Ptr TIME_STRUCT -> IO SQLUSMALLINT

foreign import ccall "odbc TIMESTAMP_STRUCT_year" odbc_TIMESTAMP_STRUCT_year
  :: Ptr TIMESTAMP_STRUCT -> IO SQLSMALLINT
foreign import ccall "odbc TIMESTAMP_STRUCT_month" odbc_TIMESTAMP_STRUCT_month
  :: Ptr TIMESTAMP_STRUCT -> IO SQLUSMALLINT
foreign import ccall "odbc TIMESTAMP_STRUCT_day" odbc_TIMESTAMP_STRUCT_day
  :: Ptr TIMESTAMP_STRUCT -> IO SQLUSMALLINT
foreign import ccall "odbc TIMESTAMP_STRUCT_hour" odbc_TIMESTAMP_STRUCT_hour
  :: Ptr TIMESTAMP_STRUCT -> IO SQLUSMALLINT
foreign import ccall "odbc TIMESTAMP_STRUCT_minute" odbc_TIMESTAMP_STRUCT_minute
  :: Ptr TIMESTAMP_STRUCT -> IO SQLUSMALLINT
foreign import ccall "odbc TIMESTAMP_STRUCT_second" odbc_TIMESTAMP_STRUCT_second
  :: Ptr TIMESTAMP_STRUCT -> IO SQLUSMALLINT
foreign import ccall "odbc TIMESTAMP_STRUCT_fraction" odbc_TIMESTAMP_STRUCT_fraction
  :: Ptr TIMESTAMP_STRUCT -> IO SQLUINTEGER

--------------------------------------------------------------------------------
-- Foreign utils

withMalloc :: Storable a => (Ptr a -> IO b) -> IO b
withMalloc m = bracket malloc free m

withMallocBytes :: Int -> (Ptr a -> IO b) -> IO b
withMallocBytes n m = bracket (mallocBytes n) free m

withCallocBytes :: Int -> (Ptr a -> IO b) -> IO b
withCallocBytes n m = bracket (callocBytes n) free m

--------------------------------------------------------------------------------
-- SQL constants

-- | hardcoded size for a GUID value,
-- https://technet.microsoft.com/en-us/library/ms172424(v=sql.110).aspx
odbcGuidBytes :: Integral a => a
odbcGuidBytes = 16

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sql.h#L50..L51
sql_success :: RETCODE
sql_success = RETCODE 0

sql_success_with_info :: RETCODE
sql_success_with_info = RETCODE 1

sql_no_data :: RETCODE
sql_no_data = RETCODE 100

sql_null_data :: SQLLEN
sql_null_data = (-1)

sql_no_total :: SQLLEN
sql_no_total = (-4)

--------------------------------------------------------------------------------
-- SQL data type constants

-- sql_unknown_type :: SQLSMALLINT
-- sql_unknown_type = 0

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

sql_type_date :: SQLSMALLINT
sql_type_date = 91

-- MS Driver-specific type
-- https://github.com/Microsoft/msphpsql/blob/master/source/shared/msodbcsql.h#L201
-- https://docs.microsoft.com/en-us/sql/relational-databases/native-client-odbc-date-time/data-type-support-for-odbc-date-and-time-improvements
sql_ss_time2 :: SQLSMALLINT
sql_ss_time2 = -154

-- sql_datetime :: SQLSMALLINT
-- sql_datetime = 9

sql_varchar :: SQLSMALLINT
sql_varchar = 12

sql_wchar :: SQLSMALLINT
sql_wchar = (-8)

sql_wvarchar :: SQLSMALLINT
sql_wvarchar = (-9)

sql_wlongvarchar :: SQLSMALLINT
sql_wlongvarchar = (-10)

-- sql_date :: SQLSMALLINT
-- sql_date = 9

-- sql_interval :: SQLSMALLINT
-- sql_interval = 10

sql_time :: SQLSMALLINT
sql_time = 10

-- sql_timestamp :: SQLSMALLINT
-- sql_timestamp = 11

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sql.h#L225..L225
sql_type_timestamp :: SQLSMALLINT
sql_type_timestamp =  93

sql_longvarchar :: SQLSMALLINT
sql_longvarchar = (-1)

sql_binary :: SQLSMALLINT
sql_binary = (-2)

sql_varbinary :: SQLSMALLINT
sql_varbinary = (-3)

-- sql_longvarbinary :: SQLSMALLINT
-- sql_longvarbinary = (-4)

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

-- sql_c_char :: SQLCTYPE
-- sql_c_char = coerce sql_char

sql_c_binary :: SQLCTYPE
sql_c_binary = coerce sql_binary

sql_c_double :: SQLCTYPE
sql_c_double = coerce sql_double

-- sql_c_float :: SQLCTYPE
-- sql_c_float = coerce sql_double

sql_c_long :: SQLCTYPE
sql_c_long = coerce sql_integer

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqlext.h#L592
sql_c_bigint :: SQLCTYPE
sql_c_bigint = coerce (sql_bigint - 20)

-- https://github.com/Microsoft/ODBC-Specification/blob/753d7e714b7eab9eaab4ad6105fdf4267d6ad6f6/Windows/inc/sqlext.h#L593
-- sql_c_biguint :: SQLCTYPE
-- sql_c_biguint = coerce (sql_bigint - 22)

sql_c_short :: SQLCTYPE
sql_c_short = coerce sql_smallint

sql_c_bit :: SQLCTYPE
sql_c_bit = coerce sql_bit

sql_c_date :: SQLCTYPE
sql_c_date = coerce (9 :: SQLSMALLINT)

sql_c_type_timestamp :: SQLCTYPE
sql_c_type_timestamp = coerce sql_type_timestamp

sql_c_time :: SQLCTYPE
sql_c_time = coerce sql_time
