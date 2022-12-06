{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Database.ODBC.TH
  ( sql
  , sqlFile
  , partsParser
  , Part(..)
  ) where

import           Control.DeepSeq
import           Data.Char
import           Data.List (foldl1')
import           Language.Haskell.TH (Q, Exp)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Text.Parsec
import           Text.Parsec.String

data Part
    = SqlPart !String
    | ParamName !String
    deriving (Show, Eq)

partsParser :: Parser [Part]
partsParser = many1 (self <|> param <|> part)
  where
    self = try (SqlPart "$" <$ string "$$") <?> "escaped dollar $$"
    param =
      (char '$' *>
       (ParamName <$>
        (many1 (satisfy isAlphaNum)) <?> "variable name (alpha-numeric only)")) <?>
      "parameter (e.g. $foo123)"
    part = (SqlPart <$> many1 (satisfy (/= '$'))) <?> "SQL code"


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
sql = QuasiQuoter { quoteExp  = buildSqlQuery "<expression>"
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
sqlFile fp = do
  !str <- fmap force (TH.runIO (readFile fp))
  buildSqlQuery fp str

buildSqlQuery :: FilePath -> String -> Q Exp
buildSqlQuery fp input = do
  case parse partsParser fp input of
    Left err    -> fail $ "Parse error in SQL: " <> show err
    Right parts -> pure $ buildExp parts

buildExp :: [Part] -> Exp
buildExp = foldl1' go . fmap toExp
  where
    toExp (SqlPart s) = TH.LitE $ TH.StringL s
    toExp (ParamName name) =
      TH.AppE (TH.VarE $ TH.mkName "toSql") (TH.VarE $ TH.mkName name)
    go a b = TH.UInfixE a (TH.VarE '(<>)) b
