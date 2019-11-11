module Bowtie.Lib.CanFailWith where

class CanFailWith e m where
  failWith :: e -> m a
