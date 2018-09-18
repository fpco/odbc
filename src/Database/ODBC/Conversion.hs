{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Conversion conveniences.

module Database.ODBC.Conversion
  ( FromValue(..)
  , FromRow(..)
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import           Data.Time
import           Data.Word
import           Database.ODBC.Internal

--------------------------------------------------------------------------------
-- Conversion from SQL

-- | Convert from a 'Value' to a regular Haskell value.
class FromValue a where
  fromValue :: Value -> Either String a
  -- ^ The 'String' is used for a helpful error message.

instance FromValue a => FromValue (Maybe a) where
  fromValue =
    \case
      NullValue -> pure Nothing
      v -> fmap Just (fromValue v)

instance FromValue Value where
  fromValue = pure

instance FromValue Text where
  fromValue =
    (\case
       TextValue x -> pure (id x)
       v -> Left ("Expected Text, but got: " ++ show v))

instance FromValue LT.Text where
  fromValue =
    (\case
       TextValue x -> pure (LT.fromStrict x)
       v -> Left ("Expected Text, but got: " ++ show v))

instance FromValue ByteString where
  fromValue =
    (\case
       ByteStringValue x -> pure (id x)
       v -> Left ("Expected ByteString, but got: " ++ show v))

instance FromValue Binary where
  fromValue =
    (\case
       BinaryValue x -> pure (id x)
       v -> Left ("Expected Binary, but got: " ++ show v))

instance FromValue L.ByteString where
  fromValue =
    (\case
       ByteStringValue x -> pure (L.fromStrict x)
       v -> Left ("Expected ByteString, but got: " ++ show v))

instance FromValue Int where
  fromValue =
    (\case
       IntValue x -> pure (id x)
       ByteValue x -> pure (fromIntegral x)
       v -> Left ("Expected Int, but got: " ++ show v))

instance FromValue Double where
  fromValue =
    (\case
       DoubleValue x -> pure (id x)
       FloatValue x -> pure (realToFrac x)
       v -> Left ("Expected Double, but got: " ++ show v))

instance FromValue Float where
  fromValue =
    (\case
       FloatValue x -> pure (realToFrac x)
       v -> Left ("Expected Float, but got: " ++ show v))

instance FromValue Word8 where
  fromValue =
    (\case
       ByteValue x -> pure x
       v -> Left ("Expected Byte, but got: " ++ show v))

instance FromValue Bool where
  fromValue =
    (\case
       BoolValue x -> pure (id x)
       v -> Left ("Expected Bool, but got: " ++ show v))

instance FromValue Day where
  fromValue =
    (\case
       DayValue x -> pure (id x)
       v -> Left ("Expected Day, but got: " ++ show v))

instance FromValue TimeOfDay where
  fromValue =
    (\case
       TimeOfDayValue x -> pure (id x)
       v -> Left ("Expected TimeOfDay, but got: " ++ show v))

instance FromValue LocalTime where
  fromValue =
    (\case
       LocalTimeValue x -> pure (id x)
       v -> Left ("Expected LocalTime, but got: " ++ show v))

--------------------------------------------------------------------------------
-- Producing rows

-- | For producing rows from a list of column values.
--
-- You can get a row of a single type like 'Text' or a list
-- e.g. @[Maybe Value]@ if you don't know what you're dealing with, or
-- a tuple e.g. @(Text, Int, Bool)@.
class FromRow r where
  fromRow :: [Value] -> Either String r

instance FromValue v => FromRow (Maybe v) where
  fromRow [NullValue] = Right Nothing
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromValue v => FromRow (Identity v) where
  fromRow [v] = fmap Identity (fromValue v)
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow [Value] where
  fromRow = pure

instance FromRow Value where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow Text where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow LT.Text where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow ByteString where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow Binary where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow L.ByteString where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow Int where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow Day where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow TimeOfDay where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow LocalTime where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow Double where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow Float where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow Word8 where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance FromRow Bool where
  fromRow [v] = fromValue v
  fromRow _ = Left "Unexpected number of fields in row"

instance (FromValue a,FromValue b) => FromRow (a,b) where
  fromRow [a,b] = (,) <$> fromValue a <*> fromValue b
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c) => FromRow (a,b,c) where
  fromRow [a,b,c] = (,,) <$> fromValue a <*> fromValue b <*> fromValue c
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d) => FromRow (a,b,c,d) where
  fromRow [a,b,c,d] = (,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e) => FromRow (a,b,c,d,e) where
  fromRow [a,b,c,d,e] = (,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f) => FromRow (a,b,c,d,e,f) where
  fromRow [a,b,c,d,e,f] = (,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g) => FromRow (a,b,c,d,e,f,g) where
  fromRow [a,b,c,d,e,f,g] = (,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h) => FromRow (a,b,c,d,e,f,g,h) where
  fromRow [a,b,c,d,e,f,g,h] = (,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i) => FromRow (a,b,c,d,e,f,g,h,i) where
  fromRow [a,b,c,d,e,f,g,h,i] = (,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j) => FromRow (a,b,c,d,e,f,g,h,i,j) where
  fromRow [a,b,c,d,e,f,g,h,i,j] = (,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k) => FromRow (a,b,c,d,e,f,g,h,i,j,k) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k] = (,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l] = (,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m] = (,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n] = (,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] = (,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o,FromValue p) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] = (,,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o <*> fromValue p
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o,FromValue p,FromValue q) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q] = (,,,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o <*> fromValue p <*> fromValue q
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o,FromValue p,FromValue q,FromValue r) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r] = (,,,,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o <*> fromValue p <*> fromValue q <*> fromValue r
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o,FromValue p,FromValue q,FromValue r,FromValue s) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s] = (,,,,,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o <*> fromValue p <*> fromValue q <*> fromValue r <*> fromValue s
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o,FromValue p,FromValue q,FromValue r,FromValue s,FromValue t) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t] = (,,,,,,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o <*> fromValue p <*> fromValue q <*> fromValue r <*> fromValue s <*> fromValue t
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o,FromValue p,FromValue q,FromValue r,FromValue s,FromValue t,FromValue u) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u] = (,,,,,,,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o <*> fromValue p <*> fromValue q <*> fromValue r <*> fromValue s <*> fromValue t <*> fromValue u
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o,FromValue p,FromValue q,FromValue r,FromValue s,FromValue t,FromValue u,FromValue v) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v] = (,,,,,,,,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o <*> fromValue p <*> fromValue q <*> fromValue r <*> fromValue s <*> fromValue t <*> fromValue u <*> fromValue v
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o,FromValue p,FromValue q,FromValue r,FromValue s,FromValue t,FromValue u,FromValue v,FromValue w) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w] = (,,,,,,,,,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o <*> fromValue p <*> fromValue q <*> fromValue r <*> fromValue s <*> fromValue t <*> fromValue u <*> fromValue v <*> fromValue w
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o,FromValue p,FromValue q,FromValue r,FromValue s,FromValue t,FromValue u,FromValue v,FromValue w,FromValue x) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x] = (,,,,,,,,,,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o <*> fromValue p <*> fromValue q <*> fromValue r <*> fromValue s <*> fromValue t <*> fromValue u <*> fromValue v <*> fromValue w <*> fromValue x
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))

instance (FromValue a,FromValue b,FromValue c,FromValue d,FromValue e,FromValue f,FromValue g,FromValue h,FromValue i,FromValue j,FromValue k,FromValue l,FromValue m,FromValue n,FromValue o,FromValue p,FromValue q,FromValue r,FromValue s,FromValue t,FromValue u,FromValue v,FromValue w,FromValue x,FromValue y) => FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y) where
  fromRow [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y] = (,,,,,,,,,,,,,,,,,,,,,,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d <*> fromValue e <*> fromValue f <*> fromValue g <*> fromValue h <*> fromValue i <*> fromValue j <*> fromValue k <*> fromValue l <*> fromValue m <*> fromValue n <*> fromValue o <*> fromValue p <*> fromValue q <*> fromValue r <*> fromValue s <*> fromValue t <*> fromValue u <*> fromValue v <*> fromValue w <*> fromValue x <*> fromValue y
  fromRow r = Left ("Unexpected number of fields in row: " ++ show (length r))
