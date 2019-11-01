module Bowtie.Infer.Elaborate where

import Bowtie.Infer.Infer
import Bowtie.Infer.Substitution
import Bowtie.Lib.Environment
import Bowtie.Lib.Id
import Bowtie.Lib.Prelude
import Bowtie.Surface.AST
import Control.Monad.State.Class
import Control.Monad.Trans.State

elaborate :: Environment -> Expr -> Either TypeError (Substitution, Type, Expr)
elaborate env expr = do
  let
    freshExpr = evalState (freshenExpr expr) 10000000 -- TODO

  case runInfer (inferType env freshExpr) of
    Left e ->
      Left e

    Right (sub, typ) ->
      pure (sub, typ, substExpr sub freshExpr)

-- | Invariant: for every Lam in the returned expression,
-- its second argument (an optional explicit type) will
-- always be 'Just'.
freshenExpr :: forall m. MonadState Int m => Expr -> m Expr
freshenExpr expr =
  case expr of
    Var _ ->
      pure expr

    Lam id mType body ->
      case mType of
        Just _ ->
          -- If there's already an explicit type on the argument, do nothing.
          pure expr

        Nothing -> do
          -- If there's no explicit type on the argument, place a new type
          -- variable there to be inferred later.
          newTypeVar <- fmap TVariable genVar
          body' <- freshenExpr body
          pure (Lam id (Just newTypeVar) body')

    App e1 e2 ->
      fmap App (freshenExpr e1) <*> freshenExpr e2

    Let bindings body ->
      let
        f :: (Expr, typ) -> m (Expr, typ)
        f (e, t) = do
          e' <- freshenExpr e
          pure (e', t)
      in
        fmap Let (for bindings f) <*> freshenExpr body

    Construct _ ->
      pure expr

    Case caseExpr alts ->
      fmap Case (freshenExpr caseExpr) <*> for alts freshenAlt

    EInt _ ->
      pure expr

    EText _ ->
      pure expr

freshenAlt :: forall m. MonadState Int m => Alt -> m Alt
freshenAlt (Alt id ids expr) =
  fmap (Alt id ids) (freshenExpr expr)
