module Bowtie.Blueprint where

import Bowtie.Lib.Prelude

import qualified Bowtie.Type.AST as Type.AST
import qualified Bowtie.Type.Parse as Type.Parse
import qualified CMark
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Blueprint
  = Blueprint (HashMap Id Type.AST.TypeDeclaration) (HashMap Id Type.AST.Type)
  deriving (Eq, Show)

data Item
  = Decl Id Type.AST.TypeDeclaration
  | Func Id Type.AST.Type

blueprint :: Text -> Either Text Blueprint
blueprint src = do
  let
    node :: CMark.Node
    node =
      CMark.commonmarkToNode mempty src

    codeBlocks :: [Text]
    codeBlocks =
      extractCode node

  code <- case codeBlocks of
            [] ->
              Left "no code found in markdown file"

            _ ->
              pure (Text.intercalate "\n" codeBlocks)

  case parseBlueprint code of
    Left e -> do
      Left (Text.pack (Mega.errorBundlePretty e))

    Right bp -> do
      pure bp

blueprintIO :: Text -> IO Blueprint
blueprintIO src =
  case blueprint src of
    Left e ->
      exitWithError e

    Right bp ->
      pure bp

conv :: [Item] -> Blueprint
conv items =
  Blueprint decls funcs
  where
    decls :: HashMap Id Type.AST.TypeDeclaration
    decls =
      let xs = mapMaybe (\a -> case a of
                                 Decl id def -> Just (id, def)
                                 Func {} -> Nothing) items
      in HashMap.fromList xs

    funcs :: HashMap Id Type.AST.Type
    funcs =
      let xs = mapMaybe (\a -> case a of
                                 Func id def -> Just (id, def)
                                 Decl {} -> Nothing) items
      in HashMap.fromList xs

parseBlueprint :: Text -> Either (Mega.ParseErrorBundle Text Void) Blueprint
parseBlueprint =
  fmap conv . Type.Parse.runParser programTypesParser "<input>"

programTypesParser :: Type.Parse.Parser [Item]
programTypesParser = do
  res <-
    many $ Mega.try $ do
      _ <- Lexer.indentGuard Type.Parse.spacesOrNewlines EQ Mega.pos1
      parseOne
  _ <- Lexer.indentGuard Type.Parse.spacesOrNewlines EQ Mega.pos1
  pure res

parseOne :: Type.Parse.Parser Item
parseOne =
  Mega.label "parseOne"
    (   Mega.try (fmap (uncurry Decl) Type.Parse.declEntryParser)
    <|> Mega.try (fmap (uncurry Func) Type.Parse.defParser)
    )

-- * Markdown

extractCode :: CMark.Node -> [Text]
extractCode (CMark.Node _ nodeType nodes) =
  fromNodeType nodeType <> concatMap extractCode nodes
  where
    fromNodeType :: CMark.NodeType -> [Text]
    fromNodeType nt =
      case nt of
        CMark.CODE_BLOCK _ t ->
          [t]

        CMark.CODE _ ->
          mempty

        _ ->
          mempty
