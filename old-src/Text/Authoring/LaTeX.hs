{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Authoring.LaTeX where

import Control.Lens
import Control.Lens.Setter
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid
import Data.Text (Text(..))

newtype LaTeX = LaTeX { _laText :: Text}
  deriving (Eq, Show)

deriving instance Monoid LaTeX

makeClassy ''LaTeX

