module Bowtie.Infer.Constraints where

import Bowtie.Infer.Substitution
import Bowtie.Lib.Prelude
import Bowtie.Lib.TypeScheme
import Bowtie.Type.AST

import qualified Data.List as List
import qualified Data.Set as Set

newtype Constraints
  = Constraints (Set Constraint)
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

data Constraint
  = EqualityConstraint Type Type
  | ExplicitInstanceConstraint Type TypeScheme
  | ImplicitInstanceConstraint Type (Set Id) Type
    -- ^ @Set Id@ are monomorphic types variables.
  deriving (Eq, Ord, Show)

toTypeSchemes :: Constraints -> Set TypeScheme
toTypeSchemes (Constraints cs) =
  Set.fromList (List.concatMap f (Set.toList cs))
  where
    f :: Constraint -> [TypeScheme]
    f c =
      case c of
        EqualityConstraint t1 t2 ->
          [TypeScheme mempty t1, TypeScheme mempty t2]

        ExplicitInstanceConstraint t ts ->
          [TypeScheme mempty t, ts]

        ImplicitInstanceConstraint t1 _ t2 ->
          [TypeScheme mempty t1, TypeScheme mempty t2]

singleton :: Constraint -> Constraints
singleton =
  Constraints . Set.singleton

add :: Constraint -> Constraints -> Constraints
add c (Constraints cs) =
  Constraints (Set.insert c cs)

isEmpty :: Constraints -> Bool
isEmpty (Constraints cs) =
  Set.null cs

subst :: Substitution -> Constraints -> Constraints
subst sub (Constraints conMap) =
  Constraints (Set.map substConstraint conMap)
  where
  substConstraint :: Constraint -> Constraint
  substConstraint c =
    case c of
      EqualityConstraint t1 t2 ->
        EqualityConstraint (substType sub t1) (substType sub t2)

      ExplicitInstanceConstraint t ts ->
        ExplicitInstanceConstraint (substType sub t) (substTypeScheme sub ts)

      ImplicitInstanceConstraint t1 monomorphicIds t2 ->
        -- TODO: is this right?
        ImplicitInstanceConstraint (substType sub t1) monomorphicIds (substType sub t2)
