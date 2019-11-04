module Bowtie.Type.Parse
  ( Parser
  , ParserErrorBundle
  , declEntryParser
  , defParser
  , typeParser
  , lowerIdParser
  , upperIdParser

  -- | Helpers
  , lexeme
  , symbol
  , spacesOrNewlines
  , parens
  , runParser
  , parseTest
  ) where

import Bowtie.Lib.Prelude hiding (many, some)
import Bowtie.Type.AST
import Control.Applicative.Combinators.NonEmpty

-- Hide @sepBy1@ because we're using the one from
-- @Control.Applicative.Combinators.NonEmpty@
-- that returns a @NonEmpty@ list instead.
import Text.Megaparsec hiding
  (State, Token, parse, parseTest, runParser, sepBy1)

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

type ParserErrorBundle = ParseErrorBundle Text Void

-- |
-- Example input:
--
-- type Foo a b = Bar Bool a | Baz Int b
--
-- Example output:
--
-- ( Foo
-- , TypeDeclaration
--     [a, b]
--     (HashMap.fromList
--       [ (Bar, [a, Bool])
--       , (Baz, [b, Int])
--       ])
-- )
--
-- >>> parseTest declEntryParser "type Foo a b = Bar Bool a | Baz Int b"
-- (Id "Foo",TypeDeclaration [Id "a",Id "b"] (fromList [(Id "Baz",[TConstructor (Id "Int"),TVariable (Id "b")]),(Id "Bar",[TConstructor (Id "Bool"),TVariable (Id "a")])]))
--
-- TODO: switch type constructors to ordered map
declEntryParser :: Parser (Id, TypeDeclaration)
declEntryParser = do
  -- Note that we don't parse a @Break@ here. If each top level
  -- parser started with Break it would actually have to be
  -- @try (toss Break)@ so they wouldn't step on each other's toes.
  symbol "type"
  typeId <- lexeme upperIdParser
  typeArgs <- many (lexeme lowerIdParser)
  symbol "="
  constructors <- constructorParser `sepBy` symbol "|"
  pure (typeId, TypeDeclaration typeArgs (HashMap.fromList constructors))

-- |
-- >>> parseTest constructorParser "Bar Bool a"
-- (Id "Bar",[TConstructor (Id "Bool"),TVariable (Id "a")])
constructorParser :: Parser (Id, [Type])
constructorParser = do
  id <- lexeme upperIdParser
  args <- many constructorArgParser
  pure (id, args)

-- |
-- For data declarations, a b c in Foo a b c
-- is parsed as three types, a, b, and c.
--
-- parseTest constructorArgParser "Foo a"
-- TypeApp (TConstructor (Id "Foo")) (TVariable (Id "a"))
--
-- >>> parseTest constructorArgParser "a"
-- TVariable (Id "a")
--
-- parseTest constructorArgParser "Maybe a"
-- TVariable (Id "a")
constructorArgParser :: Parser Type
constructorArgParser =
  label "constructorArgParser"
    (   parens typeParser
    <|> singleTypeParser'
    )

-- |
-- >>> parseTest defParser "foo : Int -> a"
-- (Id "foo",TArrow (TConstructor (Id "Int")) (TVariable (Id "a")))
defParser :: Parser (Id, Type)
defParser = do
  i <- lexeme lowerIdParser
  symbol ":"
  t <- typeParser
  pure (i, t)

-- |
-- >>> parseTest typeParser "Natural -> Int"
-- TArrow (TConstructor (Id "Natural")) (TConstructor (Id "Int"))
typeParser :: Parser Type
typeParser = do
  xs <- typeStarParser `sepBy1` symbol "->"
  -- foldr because -> is right associative:
  pure (foldr TArrow (NE.last xs) (NE.init xs))

-- | Parser for the part of a type definition separated by ->
-- in function definitions.
--
-- Foo a b c is parsed as three arguments to Foo.
--
-- parseTest typeStarParser "Maybe (Maybe a)"
-- asdf
--
-- >>> parseTest typeStarParser "(Maybe a)"
-- TypeApp (TConstructor (Id "Maybe")) (TVariable (Id "a"))
--
-- parseTest typeStarParser "Either (Maybe a) b"
-- TypeApp (TConstructor (Id "Maybe")) (TVariable (Id "a"))
--
-- >>> parseTest typeStarParser "Maybe a"
-- TypeApp (TConstructor (Id "Maybe")) (TVariable (Id "a"))
typeStarParser :: Parser Type
typeStarParser =
  label "typeStarParser"
    (   parens typeParser
    <|> singleTypeParser
    )

-- |
-- >>> parseTest singleTypeParser "Maybe a"
-- TypeApp (TConstructor (Id "Maybe")) (TVariable (Id "a"))
singleTypeParser :: Parser Type
singleTypeParser = do
  typ1 <- lexeme singleTypeParser'
  args <- many (lexeme singleTypeParser')
  -- foldl because type constructor application is left associative:
  pure (foldl' TypeApp typ1 args)

-- |
-- >>> parseTest singleTypeParser' "a"
-- TVariable (Id "a")
singleTypeParser' :: Parser Type
singleTypeParser' =
  -- TODO: a good label
      fmap TVariable (lexeme lowerIdParser)
  <|> fmap TConstructor (lexeme upperIdParser)

-- |
-- >>> parseTest lowerIdParser "a"
-- Id "a"
--
-- When we had a separate lexer this could just be tried after trying
-- to lex keyword tokens like "let" and "in". Now that we don't
-- it needs logic so that it doesn't eat those keywords.
lowerIdParser :: Parser Id
lowerIdParser = do
  notFollowedBy (keyword *> satisfy (not . validIdChar))
  c <- satisfy Char.isLower
  rest <- takeWhileP (Just "followup identifier char") validIdChar
  pure (Id (Text.cons c rest))
  where
    -- TODO: note this restiction at Id declaration
    keyword :: Parser ()
    keyword =
      void (asum (fmap chunk keywordList))

keywordList :: [Text]
keywordList =
  [ "type"
  , "let"
  , "in"
  , "case"
  , "of"
  ]

-- |
-- >>> parseTest upperIdParser "Unit"
-- Id "Unit"
upperIdParser :: Parser Id
upperIdParser = do
  c <- satisfy Char.isUpper
  rest <- takeWhileP (Just "followup identifier char") validIdChar
  pure (Id (Text.cons c rest))

-- | Internal.
validIdChar :: Char -> Bool
validIdChar =
  Char.isAlphaNum

-- * Helpers

lexeme :: Parser a -> Parser a
lexeme p = do
  -- This is an inlined an modified Text.Megaparsec.Lexer.lexeme,
  -- which is defined as:
  -- lexeme spc p = p <* spc
  res <- p
  spaces
  pure res

-- | Internal (used for 'lexeme').
spaces :: Parser ()
spaces =
  -- This is an inlined an modified Text.Megaparsec.Lexer.space,
  -- which is defined as:
  -- space ch line block = hidden . skipMany $ choice [ch, line, block]
  hidden (skipMany (space1 <|> Lexer.skipLineComment "--"))
  where
    space1 :: Parser ()
    space1 =
      void
        (takeWhile1P (Just "space character (U+0020")
        (== ' '))

symbol :: Text -> Parser ()
symbol =
  -- This is an inlined an modified Text.Megaparsec.Lexer.symbol,
  -- which is defined as:
  -- symbol spc = lexeme spc . string
  void . lexeme . chunk

spacesOrNewlines :: Parser ()
spacesOrNewlines =
  -- This is an inlined an modified Text.Megaparsec.Lexer.space.
  hidden (skipMany (spaceOrNewline1 <|> Lexer.skipLineComment "--"))
  where
    spaceOrNewline1 :: Parser ()
    spaceOrNewline1 =
      void
        (takeWhile1P
          (Just "space or newline (U+0020 or U+000A)")
          (\c -> c == ' ' || c == '\n'))

parens :: Parser a -> Parser a
parens =
  between (symbol "(") (symbol ")")

-- | Requires the parser to consume all input (unlike 'Mega.runParser').
runParser
  :: forall a. Parser a
  -> FilePath
  -> Text
  -> Either (ParseErrorBundle Text Void) a
runParser parser path =
  Mega.runParser f path
  where
    f :: Parser a
    f = do
      res <- parser
      Mega.eof
      pure res

-- | For doctests.
-- Requires the parser to consume all input (unlike 'Mega.parseTest').
parseTest
  :: (ShowErrorComponent e, Show a, Stream s)
  => Parsec e s a
  -> s
  -> IO ()
parseTest p =
  Mega.parseTest do
    res <- p
    eof
    pure res
