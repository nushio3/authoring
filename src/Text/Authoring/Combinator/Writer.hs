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
latex = scribe latexSrc

-- | Scribe a raw text (unescaped) into the target.

raw :: (MonadWriter t m, HasDocument t) => Text -> m ()
raw = latex . LTX.raw 

-- | Scribe a raw text (escaped) into the target.

esc :: (MonadWriter t m, HasDocument t) => Text -> m ()
esc = raw . LTX.protectText 

-- | Shows objects of type 'a' using Show interface,
--   without escaping LaTeX special characters.

rawShow :: (MonadWriter t m, HasDocument t, Show a) => a -> m ()
rawShow = raw . pack . Prelude.show 

-- | Shows objects of type 'a' using Show interface,
--   escaping LaTeX special characters.

escShow :: (MonadWriter t m, HasDocument t, Show a) => a -> m ()
escShow = esc . pack . Prelude.show 


