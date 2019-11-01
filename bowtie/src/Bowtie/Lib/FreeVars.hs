module Bowtie.Lib.FreeVars where

import Bowtie.Lib.Id
import Bowtie.Lib.Prelude

class FreeVars a where
  freeVars :: a -> Set Id
