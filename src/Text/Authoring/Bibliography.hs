module Text.Authoring.Bibliography where

import           Control.Lens (use)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.IO.Class
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Text.CSL.Input.Identifier (toBibTeXItem, HasDatabase)

import Text.Authoring.State

-- | generate the content for the bibliography file.

bibliographyContent :: (MonadState s m, HasAuthorState s, HasDatabase s, MonadIO m) => m Text.Text
bibliographyContent = do
  urls <- use citedUrlSet
  items <- mapM toBibTeXItem $ Set.toList urls
  return $ Text.unlines $ items
    