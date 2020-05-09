module Bowtie.Type.AST where

import Bowtie.Lib.FreeVars
import Bowtie.Lib.OrderedMap (OrderedMap)
import Bowtie.Lib.Prelude
import qualified Data.Set as Set

-- | With no items in the map this is Void, with one it's a wrapper/newtype.
--
-- Polymorphic in the variables in @[Id]@.
data TypeDeclaration
  = TypeDeclaration
      [Id]
      (OrderedMap Id [Type])
  deriving (Eq, Show)

data Type
  = TVariable Id
  | TConstructor Id
  | TArrow Type Type
  | TypeApp Type Type
  deriving (Eq, Ord, Show)

instance FreeVars Type where
  freeVars :: Type -> Set Id
  freeVars typ =
    case typ of
      TVariable id ->
        Set.singleton id
      TConstructor _id ->
        -- TODO:
        --
        -- If we return Set.singleton id here, for let x = 1 in x
        --
        -- we get
        --
        -- Right (Substitution (fromList []),TVariable (Id "0"))
        --
        -- instead of
        --
        -- inRight (Substitution (fromList []),TConstructor (Id "Int"))
        --
        -- this assumes we haven't already loaded constructors into the
        -- Environment.
        mempty
      TArrow t1 t2 ->
        freeVars t1 <> freeVars t2
      TypeApp t1 t2 ->
        freeVars t1 <> freeVars t2
