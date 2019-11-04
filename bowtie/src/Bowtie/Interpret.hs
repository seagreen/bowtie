module Bowtie.Interpret
  ( IError(..)
  , interpret
  , interpretProgram
  , sourcesToAST
  , sourcesToCore
  , concatSource
  , prettyError
  ) where

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

-- | For test and REPL use.
interpret :: Text -> Either IError Untyped.Expr
interpret src =
  interpretProgram mempty ("<input>", src)

-- | For use by the executable.
interpretProgram
  :: HashMap FilePath Text
  -> (FilePath, Text)
  -> Either IError Untyped.Expr
interpretProgram libFiles appFile = do
  (_, res) <- interpretImpl libFiles appFile
  (_, _, val) <- res
  pure val

-- | Internal.
--
-- NOTE: Environment is just the data types.
interpretImpl
  :: HashMap FilePath Text
  -> (FilePath, Text)
  -> Either
       IError
       ( AST
       , Either
           IError
           (Environment, Core.Expr, Untyped.Expr)
       )
interpretImpl libFiles appFile = do

  -- Parse
  let
    parse :: (FilePath, Text) -> Either IError AST
    parse =
      Bifunctor.first ParseError . uncurry Surface.Parse.parse

  libPrograms <- for (hashmapToSortedList libFiles) parse
  appProgram <- parse appFile

  ast <- Bifunctor.first
           NameClash
           (concatSource (libPrograms <> [appProgram])) -- PERFORMANCE

  pure (ast, inferAndEval ast)
  where
    inferAndEval :: AST -> Either IError (Environment, Core.Expr, Untyped.Expr)
    inferAndEval ast = do

      -- Kindcheck and infer
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

      -- Desugar and erase
      let
        core :: Core.Expr
        core =
          Desugar.desugar explicitlyTypedExpr

        untyped :: Untyped.Expr
        untyped =
          Erase.erase core

      -- Eval
      case Eval.eval mempty untyped of
        Left e ->
          panic ("Evaluating failed (this should never happen): " <> show e)

        Right val ->
          pure (env, core, val)

-- | For use by tests or other packages.
sourcesToAST :: HashMap FilePath Text -> (FilePath, Text) -> Either IError AST
sourcesToAST libFiles appFile = do
  (ast, _) <- interpretImpl libFiles appFile
  pure ast

-- | For use by tests or other packages.
--
-- NOTE: Environment is just the data types.
sourcesToCore
  :: HashMap FilePath Text
  -> (FilePath, Text)
  -> Either IError (Environment, Core.Expr)
sourcesToCore libFiles appFile = do
  (_, res) <- interpretImpl libFiles appFile
  (env, core, _) <- res
  pure (env, core)

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
