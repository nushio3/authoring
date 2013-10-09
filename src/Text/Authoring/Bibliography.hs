module Text.Authoring.Bibliography where

import           Control.Monad.State
import qualified Data.Text as Text

import qualified Text.Authoring.State

bibliographyContent :: (MonadState s m, HasAuthorState s) => m Text.Text
bibliographyContent = undefined