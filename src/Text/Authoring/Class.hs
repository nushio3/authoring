{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Authoring.Class where

import           Control.Lens
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Default (def)
import           Text.Authoring.Document
import           Text.Authoring.State
import           Text.LaTeX (LaTeX)
import qualified Text.CSL.Input.Identifier.Internal as Citation

-- | The all-in-one environ for writing paper.  
type MonadAuthoring s w m = (MonadState s m, HasAuthorState s, Citation.HasDatabase s, MonadWriter w m, HasDocument w, MonadIO m)  

-- | An example of monad transformer that can provide full (but IO) authoring environment.

type AuthoringT = RWST () Document AuthorState 


-- | Run an authoring monad, returns the triple (monad return value, final author state, generated latex)

runAuthoringT :: Monad m => AuthoringT m a -> m (a, AuthorState, LaTeX)
runAuthoringT prog = do 
  (a,s,w) <- runRWST prog () def
  return (a,s,w ^. latexSrc)