-- | Based on <doc.md#HeerenHagePaper>
module Bowtie.Surface.Infer where

import Bowtie.Infer.Assumptions (Assumptions)
import qualified Bowtie.Infer.Assumptions as Assumptions
import Bowtie.Infer.BottomUp
import Bowtie.Infer.Constraints
import qualified Bowtie.Infer.Elaborate as Elaborate
import Bowtie.Infer.Solve
import Bowtie.Infer.Substitution
import Bowtie.Infer.Unify
import Bowtie.Lib.CanFailWith
import Bowtie.Lib.Environment
import qualified Bowtie.Lib.Environment as Environment
import Bowtie.Lib.Prelude
import Bowtie.Surface.AST
import Control.Monad.Except
import Control.Monad.State.Class
import Control.Monad.Trans.State
import qualified Data.Set as Set

data TypeError
  = SolveStuck
  | UnifyError Type Type
  | OccursCheckFailed Id Type
  | AssumptionsRemain Assumptions
  deriving (Eq, Show)

elaborate :: Environment -> Expr -> Either TypeError (Substitution, Type, Expr)
elaborate env expr = do
  let freshExpr = evalState (Elaborate.freshenExpr expr) 10000000 -- TODO
  case runInfer (inferType env freshExpr) of
    Left e ->
      Left e
    Right (sub, typ) ->
      pure (sub, typ, substExpr sub freshExpr)

inferType ::
  ( MonadState Int m,
    CanFailWith SolveStuck m,
    CanFailWith UnifyError m,
    CanFailWith Assumptions m
  ) =>
  Environment ->
  Expr ->
  m (Substitution, Type)
inferType env expr = do
  (constraints, typ) <- gatherConstraints env expr
  s <- solve constraints
  -- Heeren paper doesn't do the substType here:
  pure (s, substType s typ)

gatherConstraints ::
  (MonadState Int m, CanFailWith Assumptions m) =>
  Environment ->
  Expr ->
  m (Constraints, Type)
gatherConstraints env expr = do
  (a, c, t) <- bottomUp env mempty expr

  -- if dom A not a subset of domain env then undefined variables exist
  let remaining :: Set Id
      remaining =
        Set.difference (Assumptions.keys a) (Environment.keys env)
  if Set.null remaining
    then pure ()
    else failWith a

  pure (c <> explicitConstraintOnSet env a, t)

-- | Heeren Section 4.5
explicitConstraintOnSet :: Environment -> Assumptions -> Constraints
explicitConstraintOnSet env a =
  Constraints (Set.fromList f)
  where
    f :: [Constraint]
    f = do
      (id, typ) <- Assumptions.toList a
      (id2, ts) <- Environment.toList env
      guard (id == id2)
      pure (ExplicitInstanceConstraint typ ts)

newtype Infer a
  = Infer (StateT Int (Either TypeError) a)
  deriving newtype (Functor, Applicative, Monad, MonadError TypeError, MonadState Int)

instance CanFailWith SolveStuck Infer where
  failWith :: SolveStuck -> Infer a
  failWith SolveStuckError =
    throwError SolveStuck

instance CanFailWith UnifyError Infer where
  failWith :: UnifyError -> Infer a
  failWith e =
    throwError
      ( case e of
          TypeMismatch t1 t2 ->
            UnifyError t1 t2
          IdOccursInType id t ->
            OccursCheckFailed id t
      )

instance CanFailWith Assumptions Infer where
  failWith :: Assumptions -> Infer a
  failWith =
    throwError . AssumptionsRemain

runInfer :: Infer a -> Either TypeError a
runInfer (Infer f) =
  evalStateT f 0
