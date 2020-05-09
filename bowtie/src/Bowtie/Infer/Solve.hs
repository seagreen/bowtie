module Bowtie.Infer.Solve where

import Bowtie.Infer.Constraints
import qualified Bowtie.Infer.Constraints as Constraints
import Bowtie.Infer.Generalize (generalize, instantiate)
import Bowtie.Infer.Substitution
import Bowtie.Infer.Unify
import Bowtie.Lib.CanFailWith
import Bowtie.Lib.Prelude
import Control.Monad.State.Class
import qualified Data.List as List
import qualified Data.Set as Set

data SolveStuck
  = SolveStuckError
  deriving (Eq, Show)

solve ::
  (MonadState Int m, CanFailWith SolveStuck m, CanFailWith UnifyError m) =>
  Constraints ->
  m Substitution
solve cs = do
  case next cs of
    Nothing ->
      if Constraints.isEmpty cs
        then pure mempty
        else failWith SolveStuckError
    Just (c, rest) -> do
      (sub, rest2) <- solveConstraint c rest
      fmap (\a -> a <> sub) (solve rest2)

solveConstraint ::
  (MonadState Int m, CanFailWith UnifyError m) =>
  Constraint ->
  Constraints ->
  m (Substitution, Constraints)
solveConstraint c rest = do
  case c of
    EqualityConstraint t1 t2 -> do
      sub <- case unify t1 t2 of
        Left unifyError ->
          failWith unifyError
        Right s ->
          pure s

      pure (sub, Constraints.subst sub rest)
    ExplicitInstanceConstraint t ts -> do
      res <- instantiate ts
      pure (mempty, Constraints.add (EqualityConstraint t res) rest)
    ImplicitInstanceConstraint t ms t2 -> do
      let res = generalize ms t2
      pure (mempty, Constraints.add (ExplicitInstanceConstraint t res) rest)

-- PERFORMANCE: everything from here down is slow.
-- Also toList isn't deterministic.

next :: Constraints -> Maybe (Constraint, Constraints)
next cs =
  asum
    [ nextEquality cs,
      nextExplicit cs,
      nextValidImplicit cs
    ]

nextEquality :: Constraints -> Maybe (Constraint, Constraints)
nextEquality (Constraints cs) = do
  res <- List.find f (Set.toList cs)
  pure (res, Constraints (Set.delete res cs))
  where
    f :: Constraint -> Bool
    f c =
      case c of
        EqualityConstraint {} ->
          True
        _ ->
          False

nextExplicit :: Constraints -> Maybe (Constraint, Constraints)
nextExplicit (Constraints cs) = do
  res <- List.find f (Set.toList cs)
  pure (res, Constraints (Set.delete res cs))
  where
    f :: Constraint -> Bool
    f c =
      case c of
        ExplicitInstanceConstraint {} ->
          True
        _ ->
          False

nextValidImplicit :: Constraints -> Maybe (Constraint, Constraints)
nextValidImplicit (Constraints cs) = do
  res <- List.find f (Set.toList cs)
  pure (res, Constraints (Set.delete res cs))
  where
    f :: Constraint -> Bool
    f c =
      -- TODO: freevars activevars check
      case c of
        ImplicitInstanceConstraint {} ->
          True
        _ ->
          False
