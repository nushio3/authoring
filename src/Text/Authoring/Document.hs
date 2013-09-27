{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Authoring.Document where

import Control.Lens.TH
import Data.Monoid
import Text.LaTeX.Base.Syntax (LaTeX)

newtype Document = Document { _latexSrc :: LaTeX }
  deriving (Eq, Show)

deriving instance Monoid Document

makeClassy ''Document
