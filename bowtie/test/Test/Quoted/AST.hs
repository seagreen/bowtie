{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Quoted.AST (quotedAST) where

import Bowtie.Surface.AST
import Bowtie.Surface.Parse
import Data.Data
import Language.Haskell.TH.Quote
import Prelude
import Test.Quoted.Expr (liftDataWithText, trimLeadingNewline)

import qualified Data.Text as Text

deriving stock instance Data AST
deriving stock instance Data TypeDeclaration

quotedAST :: QuasiQuoter
quotedAST =
  QuasiQuoter
    { quoteExp =
        liftDataWithText . dirtyParseAST . Text.pack . trimLeadingNewline

    , quotePat = \_ -> error "quotedAST: quotePat not defined"
    , quoteType = \_ -> error "quotedAST: quoteType not defined"
    , quoteDec  = \_ -> error "quotedAST: quoteDec not defined"
    }
