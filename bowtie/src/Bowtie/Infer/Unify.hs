module Bowtie.Infer.Unify where

import Bowtie.Infer.Substitution
import Bowtie.Lib.FreeVars
import Bowtie.Lib.Id
import Bowtie.Lib.Prelude
import Bowtie.Surface.AST

import qualified Data.Set as Set

data UnifyError
  = UnifyError Type Type
  | OccursCheckFailed Id Type
  deriving (Eq, Show)

-- | Implementation based on <doc.md#AlgorithmWStepByStep>.
unify :: Type -> Type -> Either UnifyError Substitution
unify t1 t2 =

  -- For example consider:
  --
  --   (\x -> x) 5
  --
  -- Which results in one contraint:
  --
  --   1->1 == Int->0
  --
  -- with the type:
  --
  --   0
  --
  -- unify is called with @TArrow 1 1@ and @TArrow Int 0@ as its arguments.
  --
  -- 1. Unify hits the @TArrow arg1 res1, TArrow arg2 res2@ case,
  -- and recurses with the arguments 1 and Int.
  --
  -- 2. This results in a substitution of 1 for Int.
  --
  -- 3. Unify begins the second resursion, with the arguments 1 and 0,
  -- but before it does so it applies the substitution to them,
  -- resulting in Int and 0.
  --
  -- 4. This results in a substitution of 0 for Int.
  --
  -- 5. It returns the composition of these, substituting Int for 1,
  -- followed by Int for 0.

  if t1 == t2
    then
      Right mempty

    else
      case (t1, t2) of
        (TVariable id, _) ->
          unifyVariable id t2

        (_, TVariable id) ->
          unifyVariable id t1

        (TArrow arg1 res1, TArrow arg2 res2) -> do
          s1 <- unify arg1 arg2
          s2 <- unify (substType s1 res1) (substType s1 res2)
          Right (s1 <> s2)

        (TypeApp a1 b1, TypeApp a2 b2) -> do
          s1 <- unify a1 a2
          s2 <- unify (substType s1 b1) (substType s1 b2)
          Right (s1 <> s2)

        _ ->
          Left (UnifyError t1 t2)
  where
    unifyVariable :: Id -> Type -> Either UnifyError Substitution
    unifyVariable id typ =
      if Set.member id (freeVars typ)
        then
          Left (OccursCheckFailed id typ)

        else
          Right (singleSub id typ)
