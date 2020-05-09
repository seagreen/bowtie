module Bowtie.Infer.Elaborate where

import Bowtie.Lib.Prelude
import Bowtie.Surface.AST
import Control.Monad.State.Class

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
      let f :: (Expr, typ) -> m (Expr, typ)
          f (e, t) = do
            e' <- freshenExpr e
            pure (e', t)
       in fmap Let (for bindings f) <*> freshenExpr body
    Construct _ ->
      pure expr
    Case caseExpr alts ->
      fmap Case (freshenExpr caseExpr) <*> for alts freshenAlt
    IntLiteral _ ->
      pure expr
    TextLiteral _ ->
      pure expr

freshenAlt :: forall m. MonadState Int m => Alt -> m Alt
freshenAlt (Alt id ids expr) =
  fmap (Alt id ids) (freshenExpr expr)
