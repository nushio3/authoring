module Text.Authoring.Bibliography (bibliographyContent, toBibliographyContent, withDatabaseFile) where

import           Control.Lens (use)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.IO.Class
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Text.CSL.Input.Identifier (toBibTeXItem, HasDatabase, withDatabaseFile)

import Text.Authoring.State

-- | generate the content of the LaTeX bibliography @.bib@ file.
--   We need 'MonadIO' capability in order to access online 
--   literature information.

bibliographyContent :: (MonadState s m, HasAuthorState s, HasDatabase s, MonadIO m) => m Text.Text
bibliographyContent = do
  urls <- use citedUrlSet
  items <- mapM toBibTeXItem $ Set.toList urls
  return $ Text.unlines $ items
    
    

-- | generate the @.bib@ file outside the authoring monad.
toBibliographyContent :: (HasAuthorState s, HasDatabase s, MonadIO m) => 
                         s -> m Text.Text
toBibliographyContent s0 = flip evalStateT s0 $ do
  urls <- use citedUrlSet
  items <- mapM toBibTeXItem $ Set.toList urls
  return $ Text.unlines $ items
