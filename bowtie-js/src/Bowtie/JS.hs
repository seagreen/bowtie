module Bowtie.JS where

import Bowtie.Core.Expr
import Bowtie.Lib.Id
import Bowtie.Lib.OrderedMap (OrderedMap)
import Bowtie.Lib.Prelude

import qualified Bowtie.Lib.OrderedMap as OrderedMap
import qualified Data.HashMap.Strict as HashMap

appendConsoleLog :: Text -> Text
appendConsoleLog js =
  js <> "\n\nconsole.log(result);"

-- | I initially though packageUp's purpose would be to make sure
-- it's being passed a full program, which should be a Let,
-- and it would return Nothing otherwise.
--
-- Then I realized that even full programs can be non-Lets,
-- because if the whole program is:
--
-- result : Int -> Int
-- result =
--   (\n. n)
--
-- then it desugars to
--
-- Lambda n (Id n)
--
-- with no Let in sight (basically any program that has a single "result"
-- definition doesn't have to be a Let).
packageUp :: Expr -> (OrderedMap Id Expr, Expr)
packageUp expr =
  case expr of
    Let bindings body ->
      case OrderedMap.fromList (HashMap.toList (fmap fst bindings)) of
        Left _ ->
          panic "shouldn't happen"

        Right oBindings ->
          let (more, finalBody) = packageUp body
          in case OrderedMap.append oBindings more of
            Left _ ->
              panic "also shouldn't happen"

            Right res ->
              (res, finalBody)

    _ ->
      (OrderedMap.empty, expr)
