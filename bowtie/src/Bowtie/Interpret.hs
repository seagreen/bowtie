module Bowtie.Interpret where

import Bowtie.Lib.Environment
import Bowtie.Lib.Prelude
import Bowtie.Surface.AST (AST(astTerms, astTypes))
import Bowtie.Type.Kindcheck
import Bowtie.Type.Parse (ParserErrorBundle)

import qualified Bowtie.Core.Expr as Core
import qualified Bowtie.Surface.AST as Surface
import qualified Bowtie.Surface.Desugar as Desugar
import qualified Bowtie.Surface.Infer as Infer
import qualified Bowtie.Surface.Parse as Surface.Parse
import qualified Bowtie.Untyped.Erase as Erase
import qualified Bowtie.Untyped.Eval as Eval
import qualified Bowtie.Untyped.Expr as Untyped
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega

data IError
  = ParseError ParserErrorBundle
  | NameClash Text
  | TypeError Infer.TypeError
  deriving (Eq, Show)

interpret :: Text -> Either IError Untyped.Expr
interpret src =
  interpretProgram mempty ("<input>", src)

interpretProgram
  :: HashMap FilePath Text
  -> (FilePath, Text)
  -> Either IError Untyped.Expr
interpretProgram libFiles appFile = do
  (_, core) <- sourcesToCore libFiles appFile
  let
    untyped :: Untyped.Expr
    untyped =
      Erase.erase core

  case Eval.eval mempty untyped of
    Left e ->
      panic ("Evaluating failed (this should never happen): " <> show e)

    Right a ->
      pure a

sourcesToAST :: HashMap FilePath Text -> (FilePath, Text) -> Either IError AST
sourcesToAST libFiles appFile = do
  libPrograms <- Bifunctor.first ParseError (for (hashmapToSortedList libFiles) parse)
  appProgram <- Bifunctor.first ParseError (parse appFile)
  Bifunctor.first NameClash (concatSource (libPrograms <> [appProgram])) -- PERFORMANCE
  where
    parse :: (FilePath, Text) -> Either ParserErrorBundle AST
    parse =
      uncurry Surface.Parse.parse

-- | NOTE: Environment is just the data types.
sourcesToCore :: HashMap FilePath Text -> (FilePath, Text) -> Either IError (Environment, Core.Expr)
sourcesToCore libFiles appFile = do
  ast <- sourcesToAST libFiles appFile
  let
    env :: Environment
    env =
      kindcheck (astTypes ast)

    dsg :: Surface.Expr
    dsg =
      Desugar.extractResult (astTerms ast)

  (_, _, explicitlyTypedExpr) <- Bifunctor.first
                                   TypeError
                                   (Infer.elaborate env dsg)
  pure (env, Desugar.desugar explicitlyTypedExpr)

concatSource :: [AST] -> Either Text AST
concatSource =
  foldM f Surface.emptyAST
  where
    f :: AST -> AST -> Either Text AST
    f s1 s2 =
      case Surface.appendAST s1 s2 of
        Left e ->
          -- TODO: show names of modules
          Left ("Duplicate definitions found in multiple modules with name " <> show e)

        Right a ->
          pure a

prettyError :: IError -> Text
prettyError err =
  case err of
    ParseError e ->
      "Parse error: " <> Text.pack (Mega.errorBundlePretty e)

    NameClash t ->
      t

    TypeError e ->
      "Type error: " <> show e
