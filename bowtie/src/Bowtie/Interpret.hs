module Bowtie.Interpret where

import Bowtie.Infer.Infer (TypeError)
import Bowtie.Lib.Environment
import Bowtie.Lib.Prelude
import Bowtie.Surface.AST (AST(astTerms, astTypes))
import Bowtie.Type.Kindcheck
import Bowtie.Type.Parse (ParserErrorBundle)
import System.Directory (listDirectory)
import System.FilePath (FilePath, (</>))

import qualified Bowtie.Core.Expr as Core
import qualified Bowtie.Infer.Elaborate as Infer.Elaborate
import qualified Bowtie.Surface.AST as Surface
import qualified Bowtie.Surface.Desugar as Surface.Desugar
import qualified Bowtie.Surface.Parse as Surface.Parse
import qualified Bowtie.Untyped.Erase as Untyped.Erase
import qualified Bowtie.Untyped.Eval as Untyped.Eval
import qualified Bowtie.Untyped.Expr as Untyped
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as Mega

interpret :: Text -> Either IError Untyped.Expr
interpret src = do
  (_, core) <- sourceToCore src
  let
    untyped :: Untyped.Expr
    untyped =
      Untyped.Erase.erase core

  case Untyped.Eval.eval mempty untyped of
    Left e ->
      panic ("Evaluating failed (this should never happen): " <> show e)

    Right a ->
      pure a

interpretProgram :: [(FilePath, Text)] -> (FilePath, Text) -> Either IError Untyped.Expr
interpretProgram libFiles appFile = do
  (_, core) <- sourcesToCore libFiles appFile
  let
    untyped :: Untyped.Expr
    untyped =
      Untyped.Erase.erase core

  case Untyped.Eval.eval mempty untyped of
    Left e ->
      panic ("Evaluating failed (this should never happen): " <> show e)

    Right a ->
      pure a

data IError
  = ParseError ParserErrorBundle
  | NameClash Text
  | TypeError TypeError
  deriving (Eq, Show)

prettyError :: IError -> Text
prettyError err =
  case err of
    ParseError e ->
      "Parse error: " <> Text.pack (Mega.errorBundlePretty e)

    NameClash t ->
      t

    TypeError e ->
      "Type error: " <> show e

-- | NOTE: Environment is just the data types.
sourceToCore :: Text -> Either IError (Environment, Core.Expr)
sourceToCore src = do
  ast <- Bifunctor.first ParseError (Surface.Parse.parse "<input>" src)
  let
    env :: Environment
    env =
      kindcheck (astTypes ast)

    dsg :: Surface.Expr
    dsg =
      Surface.Desugar.desugarResult (astTerms ast)

  (_, _, explicitlyTypedExpr) <- Bifunctor.first
                                   TypeError
                                   (Infer.Elaborate.elaborate env dsg)

  pure (env, Surface.Desugar.dsg explicitlyTypedExpr)

sourcesToAST :: [(FilePath, Text)] -> (FilePath, Text) -> Either IError AST
sourcesToAST libFiles appFile = do
  libPrograms <- Bifunctor.first ParseError (for libFiles parse)
  appProgram <- Bifunctor.first ParseError (parse appFile)
  Bifunctor.first NameClash (concatSource (libPrograms <> [appProgram]))
  where
    parse :: (FilePath, Text) -> Either ParserErrorBundle AST
    parse =
      uncurry Surface.Parse.parse

-- | NOTE: Environment is just the data types.
sourcesToCore :: [(FilePath, Text)] -> (FilePath, Text) -> Either IError (Environment, Core.Expr)
sourcesToCore libFiles appFile = do
  ast <- sourcesToAST libFiles appFile
  let
    env :: Environment
    env =
      kindcheck (astTypes ast)

    dsg :: Surface.Expr
    dsg =
      Surface.Desugar.desugarResult (astTerms ast)

  (_, _, explicitlyTypedExpr) <- Bifunctor.first
                                   TypeError
                                   (Infer.Elaborate.elaborate env dsg)
  pure (env, Surface.Desugar.dsg explicitlyTypedExpr)

getLibFiles :: FilePath -> IO [(FilePath, Text)]
getLibFiles dir = do
  paths <- (fmap.fmap) (\p -> dir </> p) (listDirectory dir)
  for paths f
    where
      f :: FilePath -> IO (FilePath, Text)
      f path = do
        t <- TIO.readFile path
        pure (path, t)

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
