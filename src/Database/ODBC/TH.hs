module Database.ODBC.TH
  ( sql
  , sqlFile
  ) where

import           Control.Monad ((<=<))
import           Data.Attoparsec.ByteString.Char8 (Parser, atEnd, takeTill, parseOnly)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Char (isSpace)
import           Data.List (foldl1')
import           Data.Monoid ((<>))
import           Language.Haskell.TH (Q, Exp)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote (QuasiQuoter(..))

data Part
    = SqlPart !ByteString
    | ParamName !ByteString

parseParts :: Parser [Part]
parseParts = do
  eof <- atEnd

  if eof
    then pure []
    else do
      txt <- takeTill (== '$')
      end <- atEnd

      if end
        then pure [SqlPart txt]
        else do
          name <- takeTill isSpace
          let prepended xs = SqlPart txt : ParamName name : xs
          fmap prepended parseParts

{- | Allows SQL parameters interpolation from a SQL query. Only 'quoteExp' is
     implemented because this quote can only be used at the expression level.

@
select_some_stuff :: Text -> Int -> Query
select_some_stuff name age = [sql|select * from user where age = $age AND name = $name|]
@

     In this case, 'sql' quote will generate the code below:

@
"select * from user where age = " 'Data.Monoid.<>' 'Database.ODBC.SQLServer.toSql' age 'Data.Monoid.<>' " AND name = " 'Data.Monoid.<>' 'Database.ODBC.SQLServer.toSql' name
@
-}
sql :: QuasiQuoter
sql = QuasiQuoter { quoteExp  = parseSqlParams
                  , quotePat  = ignored
                  , quoteType = ignored
                  , quoteDec  = ignored
                  }
  where
    ignored :: x -> Q a
    ignored _ = fail "sql quote can be used at the expression level only"

{- | Loads the content of a SQL query file and allows SQL parameters interpolation
     from it.

@
select_some_stuff :: Text -> Int -> Query
select_some_stuff name age = $(sqlFile "path\/to\/my\/sql\/file.sql")
@

     See 'sql' for more details.
-}
sqlFile :: FilePath -> Q Exp
sqlFile = buildSqlQuery <=< TH.runIO . ByteString.readFile

parseSqlParams :: String -> Q Exp
parseSqlParams = buildSqlQuery . ByteString.pack

buildSqlQuery :: ByteString -> Q Exp
buildSqlQuery input = do
  case parseOnly parseParts input of
    Left err    -> fail $ "Impossible happened while parsing sql parts: " <> err
    Right parts -> pure $ buildExp parts

buildExp :: [Part] -> Exp
buildExp = foldl1' go . fmap toExp
  where
    toExp (SqlPart s) =
        TH.LitE $ TH.StringL $ ByteString.unpack s
    toExp (ParamName name) =
        TH.AppE (TH.VarE $ TH.mkName "toSql")
                (TH.VarE $ TH.mkName $ ByteString.unpack $ ByteString.tail name)

    go a b = TH.UInfixE a (TH.VarE $ TH.mkName "<>") b

