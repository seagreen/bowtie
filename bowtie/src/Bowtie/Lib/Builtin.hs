module Bowtie.Lib.Builtin where

import Bowtie.Lib.Prelude

int :: Id
int =
  Id "Int"

text :: Id
text =
  Id "Text"

unicode :: Id
unicode =
  Id "Unicode"

nil :: Id
nil =
  Id "Nil"

cons :: Id
cons =
  Id "Cons"

lessThan :: Id
lessThan =
  Id "LessThan"

equal :: Id
equal =
  Id "Equal"

greaterThan :: Id
greaterThan =
  Id "GreaterThan"
