{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Authoring.Class where

import           Control.Monad.State
import           Control.Monad.Writer
import           Text.Authoring.Document
import           Text.Authoring.State
import qualified Text.CSL.Input.Identifier.Internal as Citation

-- | The all-in-one environ for writing paper.  
type MonadAuthor s w m = (MonadState s m, HasAuthorState s, Citation.HasDatabase s, MonadWriter w m, HasDocument w, MonadIO m)  
