{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Authoring.Class where

import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Default (def)
import           Text.Authoring.Document
import           Text.Authoring.State
import qualified Text.CSL.Input.Identifier.Internal as Citation

-- | The all-in-one environ for writing paper.  
type MonadAuthoring s w m = (MonadState s m, HasAuthorState s, Citation.HasDatabase s, MonadWriter w m, HasDocument w, MonadIO m)  

-- | An example of monad transformer that can provide authoring environment.

type AuthoringT = RWST () Document AuthorState 

runAuthoringT :: AuthoringT m a -> m (a, AuthorState, Document)
runAuthoringT prog = runRWST prog () def