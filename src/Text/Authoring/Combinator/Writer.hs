{-# LANGUAGE MultiParamTypeClasses #-}

-- | Names of this module are intentitionally kept too short.
--   Use this module with quantifier.

module Text.Authoring.Combinator.Writer where

import Control.Lens (scribe)
import Control.Monad.Writer
import Data.Text (Text, pack)
import Prelude 
import Safe (readMay)
import Text.LaTeX.Base.Syntax (LaTeX)
import qualified Text.LaTeX.Base.Commands as LTX 
import qualified Text.LaTeX.Base.Render as LTX 
import qualified Text.LaTeX.Base.Syntax as LTX 
import qualified Text.LaTeX.Base.Texy as LTX 


import Text.Authoring.Document


-- | Scribe a LaTeX syntax into the target document.

latex :: (MonadWriter t m, HasDocument t) => LaTeX -> m ()
latex x = scribe latexSrc x

-- | Scribe a raw text (unescaped) into the target.

raw :: (MonadWriter t m, HasDocument t) => Text -> m ()
raw x = latex $ LTX.raw x


-- | Shows objects of type 'a' using Show interface
--   Escapes LaTeX special characters.

esc :: (MonadWriter t m, HasDocument t, Show a) => a -> m ()
esc x = raw $ LTX.protectText $ pack $ Prelude.show x

