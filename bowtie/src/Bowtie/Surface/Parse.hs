-- So that we can use parseTest in doctests without complaint:
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Bowtie.Surface.Parse
   ( parse
   , exprParser

   -- | For test and REPL use
   , dirtyParseExpr
   , dirtyParseAST
   ) where

import Bowtie.Lib.OrderedMap (OrderedMap)
import Bowtie.Lib.Prelude hiding (many, some)
import Bowtie.Surface.AST
import Bowtie.Type.Parse
  (Parser, ParserErrorBundle, lexeme, lowerIdParser, parseTest,
  spacesOrNewlines, symbol, upperIdParser)
import Control.Applicative.Combinators.NonEmpty
import Text.Megaparsec hiding (parse, parseTest, some)

import qualified Bowtie.Lib.OrderedMap as OrderedMap
import qualified Bowtie.Type.Parse as Type
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Prelude
import qualified Text.Megaparsec.Char.Lexer as Lexer

parse :: FilePath -> Text -> Either ParserErrorBundle AST
parse path =
  Type.runParser sourceParser path

-- |
-- >>> parseTest sourceParser "type A = A\n\ntype B = B"
-- AST {astTypes = OrderedMap (fromList [(Id "A",(0,TypeDeclaration [] (OrderedMap (fromList [(Id "A",(0,[]))]) (fromList [(0,(Id "A",[]))])))),(Id "B",(1,TypeDeclaration [] (OrderedMap (fromList [(Id "B",(0,[]))]) (fromList [(0,(Id "B",[]))]))))]) (fromList [(0,(Id "A",TypeDeclaration [] (OrderedMap (fromList [(Id "A",(0,[]))]) (fromList [(0,(Id "A",[]))])))),(1,(Id "B",TypeDeclaration [] (OrderedMap (fromList [(Id "B",(0,[]))]) (fromList [(0,(Id "B",[]))]))))]), astTerms = OrderedMap (fromList []) (fromList [])}
--
-- ^^ TODO: OrderedMaps do not do well with Show
sourceParser :: Parser AST
sourceParser = do

  -- This was my other attempt at this,
  -- but the "try" made for terrible errors
  --
  -- _ <- Lexer.indentGuard spacesOrNewlines EQ pos1
  -- entries <-
  --   many $ try $ do
  --     _ <- Lexer.indentGuard spacesOrNewlines EQ pos1
  --     entryParser

  _ <- Lexer.indentGuard spacesOrNewlines EQ pos1
  entries <-
    many do
      e <- entryParser
      -- TODO: ensure that we actually did get an eof at the end of the list
      void (Lexer.indentGuard spacesOrNewlines EQ pos1) <|> eof
      pure e

  foldM f emptyAST entries
  where
    f :: AST -> AST -> Parser AST
    f s1 s2 =
      case appendAST s1 s2 of
        Left (TypeIdConflict id) ->
          fail ("Duplicate type definitions found in module with name " <> Text.unpack (unId id))

        Left (TermIdConflict id) ->
          fail ("Duplicate type definitions found in module with name " <> Text.unpack (unId id))

        Right a ->
          pure a

entryParser :: Parser AST
entryParser =
  label "entryParser"
    (   fmap (\(i,d) -> AST (OrderedMap.singleton i d) OrderedMap.empty) Type.declEntryParser
    <|> fmap (\(i,e,typ) -> AST OrderedMap.empty (OrderedMap.singleton i (e, typ))) dParser
    )

-- |
-- >>> parseTest dParser "a : Int\na =\n  1"
-- (Id "a",IntLiteral 1,TConstructor (Id "Int"))
dParser :: Parser (Id, Expr, Type)
dParser = do
  pos <- Lexer.indentLevel
  (id, typ) <- Type.defParser
  _ <- Lexer.indentGuard spacesOrNewlines EQ pos
  (id2, expr) <- valDefParser
  when
    (id /= id2)
    (fail (Text.unpack ("Type and term IDs don't match: " <> unId id <> " " <> unId id2)))
  pure (id, expr, typ)

-- |
-- >>> parseTest valDefParser "a =\n  1"
-- (Id "a",IntLiteral 1)
valDefParser :: Parser (Id, Expr)
valDefParser = do
  pos <- Lexer.indentLevel
  i <- lexeme lowerIdParser
  symbol "="
  _ <- Lexer.indentGuard spacesOrNewlines GT pos
  e <- exprParser
  pure (i, e)

exprParser :: Parser Expr
exprParser =
  label "exprParser"
    (   lamParser
    <|> letParser
    <|> caseParser
    <|> listToAppParser
    )

-- |
-- >>> parseTest lamParser "\\x : Int. x"
-- Lam (Id "x") (Just (TConstructor (Id "Int"))) (Var (Id "x"))
lamParser :: Parser Expr
lamParser = do
  pos <- Lexer.indentLevel
  symbol "\\"
  id <- lexeme lowerIdParser
  mType <- optional annotationParser
  symbol "."
  _ <- Lexer.indentGuard spacesOrNewlines GT pos
  expr <- exprParser
  pure (Lam id mType expr)
  where
    annotationParser :: Parser Type
    annotationParser = do
      symbol ":"
      Type.typeParser

-- |
-- parseTest letParser "let\n  a = b\nin\n  a"
-- Case (Var (Id "a")) [Alt (Id "True") [] (IntLiteral 1),Alt (Id "False") [] (IntLiteral 2)]
letParser :: Parser Expr
letParser = do
  pos <- Lexer.indentLevel
  decls <- Lexer.indentBlock spacesOrNewlines p
  _ <- Lexer.indentGuard spacesOrNewlines EQ pos
  symbol "in"
  _ <- Lexer.indentGuard spacesOrNewlines GT pos
  e <- exprParser
  pure (Let decls e)
  where
    p :: Parser (Lexer.IndentOpt Parser (OrderedMap Id (Expr, Type)) (Id, Expr, Type))
    p = do
      symbol "let"
      pure (Lexer.IndentSome Nothing g dParser)

    g :: [(Id, Expr, Type)] -> Parser (OrderedMap Id (Expr, Type))
    g decls =
      let
        f :: (Id, Expr, Type) -> (Id, (Expr, Type))
        f (i,e,typ) = (i, (e, typ))
      in
        case OrderedMap.fromList (fmap f decls) of
          Left id ->
            fail ("Duplicate identifiers found in let expression with id: " <> Text.unpack (unId id))

          Right a ->
            pure a

-- |
-- >>> parseTest caseParser "case a of\n  True -> 1\n\n  False -> 2"
-- Case (Var (Id "a")) [Alt (Id "True") [] (IntLiteral 1),Alt (Id "False") [] (IntLiteral 2)]
caseParser :: Parser Expr
caseParser =
  Lexer.indentBlock spacesOrNewlines p
  where
    p :: Parser (Lexer.IndentOpt Parser Expr Alt)
    p = do
      -- Each item in this block must be indented more than this token
      -- (and they must all be indented at the same level):
      symbol "case"
      e <- lexeme exprParser
      symbol "of"
      pure (Lexer.IndentMany Nothing (\alts -> pure (Case e alts)) altParser)

-- |
-- >>> parseTest altParser "Just a -> a"
-- Alt (Id "Just") [Id "a"] (Var (Id "a"))
altParser :: Parser Alt
altParser = do
  pos <- Lexer.indentLevel
  i <- lexeme upperIdParser
  ids <- many (lexeme lowerIdParser)
  symbol "->"
  _ <- Lexer.indentGuard spacesOrNewlines GT pos
  e <- exprParser
  pure (Alt i ids e)

-- |
-- >>> parseTest listToAppParser "double 1"
-- App (Var (Id "double")) (IntLiteral 1)
listToAppParser :: Parser Expr
listToAppParser = do
  pos <- Lexer.indentLevel -- Could use 'lineFold' here
  func <- lexeme itemParser
  args <- many (f pos)
  pure (foldl' App func args) -- foldl because App is left associative
  where
    f :: Pos -> Parser Expr
    f pos = do
      _ <- try (Lexer.indentGuard spacesOrNewlines GT pos)
      lexeme itemParser

itemParser :: Parser Expr
itemParser =
  label "itemParser"
    (   Type.parens exprParser
    <|> varParser
    <|> conParser
    <|> intParser
    <|> textParser
    )

varParser :: Parser Expr
varParser =
  fmap Var (lexeme lowerIdParser)

conParser :: Parser Expr
conParser =
  fmap Construct (lexeme upperIdParser)

-- |
-- >>> parseTest intParser "1"
-- IntLiteral 1
--
-- >>> parseTest intParser "-1"
-- IntLiteral (-1)
intParser :: Parser Expr
intParser = do
  mNegate <- optional (single '-')
  digits <- some (satisfy Char.isDigit)
  pure (IntLiteral (Prelude.read (f mNegate (NonEmpty.toList digits)))) -- TODO: read
  where
    f :: Maybe Char -> [Char] -> [Char]
    f mNegate digits =
      case mNegate of
        Nothing ->
          digits

        Just _ ->
          '-' : digits

-- |
-- >>> parseTest textParser "\"abc\""
-- TextLiteral "abc"
textParser :: Parser Expr
textParser = do
  _ <- single '"'
  t <- takeWhileP (Just "Text char") (/= '"')
  _ <- single '"'
  pure (TextLiteral t)

-- * Utils

-- | For test and REPL use.
dirtyParseExpr :: Text -> Expr
dirtyParseExpr src =
  case Type.runParser exprParser "<source_code>" src of
    Left e ->
      panic (Text.pack (errorBundlePretty e))

    Right expr ->
      expr

-- | For test and REPL use.
dirtyParseAST :: Text -> AST
dirtyParseAST src =
  case parse "<source_code>" src of
    Left e ->
      panic (Text.pack (errorBundlePretty e))

    Right program ->
      program
