module Bowtie.Type.Kindcheck where

import Bowtie.Lib.Environment
import Bowtie.Lib.OrderedMap (OrderedMap)
import Bowtie.Lib.Prelude
import Bowtie.Lib.TypeScheme
import Bowtie.Type.AST

import qualified Bowtie.Lib.OrderedMap as OrderedMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

-- | TODO: Actually kindcheck!
kindcheck :: OrderedMap Id TypeDeclaration -> Environment
kindcheck =
  Environment . HashMap.fromList . foldMap constructorsFromDecls . OrderedMap.toList
  where
    -- Note that these Ids are different.
    -- E.g. if the first is Maybe, the second is Nothing and Just.
    constructorsFromDecls :: (Id, TypeDeclaration) -> [(Id, TypeScheme)]
    constructorsFromDecls (typeId, TypeDeclaration polyVars constructors) =
      fmap constructorType (OrderedMap.toList constructors)
      where
        constructorType :: (Id, [Type]) -> (Id, TypeScheme)
        constructorType (conId, args) =
          let
            finalType :: Type
            finalType =
              foldr f (TConstructor typeId) polyVars
              where
                f :: Id -> Type -> Type
                f id acc =
                  TypeApp acc (TVariable id)

            typ :: Type
            typ =
              foldr TArrow finalType args
          in
            ( conId
            , TypeScheme (Set.fromList polyVars) typ
            )
