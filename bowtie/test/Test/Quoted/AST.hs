{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Quoted.AST (quotedAST) where

import Bowtie.Lib.Prelude
import Bowtie.Surface.AST
import Bowtie.Surface.Parse
import Data.Data
import qualified Data.Text as Text
import Language.Haskell.TH.Quote
import Test.Quoted.Expr (liftDataWithText, trimLeadingNewline)

deriving stock instance Data AST

deriving stock instance Data TypeDeclaration

quotedAST :: QuasiQuoter
quotedAST =
  QuasiQuoter
    { quoteExp =
        liftDataWithText . dirtyParseAST . Text.pack . trimLeadingNewline,
      quotePat = \_ -> panic "quotedAST: quotePat not defined",
      quoteType = \_ -> panic "quotedAST: quoteType not defined",
      quoteDec = \_ -> panic "quotedAST: quoteDec not defined"
    }
