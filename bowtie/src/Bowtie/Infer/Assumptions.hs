module Bowtie.Infer.Assumptions where

import Bowtie.Lib.Id
import Bowtie.Lib.Prelude
import Bowtie.Type.AST

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

-- | A multiset.
newtype Assumptions
  = Assumptions (HashMap Id (Set Type))
  deriving stock (Eq, Show)
  deriving newtype (Monoid)

instance Semigroup Assumptions where
  Assumptions s1 <> Assumptions s2 =
    -- <> is `union`, which in case of conflict uses the first map.
    Assumptions (HashMap.unionWith (<>) s1 s2)

delete :: Id -> Assumptions -> Assumptions
delete id (Assumptions a) =
  Assumptions (HashMap.delete id a)

toList :: Assumptions -> [(Id, Type)]
toList (Assumptions a) =
  concatMap f (HashMap.toList a)
  where
    f :: (Id, Set Type) -> [(Id, Type)]
    f (i, ts) =
      fmap (\t -> (i, t)) (Set.toList ts)

singleton :: Id -> Type -> Assumptions
singleton id typ =
  Assumptions (HashMap.singleton id (Set.singleton typ))
