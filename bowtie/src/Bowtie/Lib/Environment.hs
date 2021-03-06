-- | Used by both the types code (as the result of kindchecking)
-- and by the inference code.
module Bowtie.Lib.Environment where

import Bowtie.Lib.FreeVars
import Bowtie.Lib.Prelude
import Bowtie.Lib.TypeScheme
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

-- | (A type environment, not a term environment like appear elsewhere
-- in the code)
newtype Environment = Environment {unEnvironment :: HashMap Id TypeScheme}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance FreeVars Environment where
  freeVars :: Environment -> Set Id
  freeVars (Environment env) =
    foldMap freeVars (HashMap.elems env)

lookup :: Id -> Environment -> Maybe TypeScheme
lookup id env =
  HashMap.lookup id (unEnvironment env)

addBinding :: Id -> TypeScheme -> Environment -> Environment
addBinding id t env =
  Environment (HashMap.insert id t (unEnvironment env))

keys :: Environment -> Set Id
keys (Environment env) =
  Set.fromList (HashMap.keys env)

toList :: Environment -> [(Id, TypeScheme)]
toList (Environment env) =
  HashMap.toList env
