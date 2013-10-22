{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Authoring.Combinator.Cite where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.CSL.Input.Identifier (resolve, HasDatabase)

import Text.Authoring.Class
import Text.Authoring.Combinator.Meta
import Text.Authoring.Combinator.Writer
import Text.Authoring.Document
import Text.Authoring.State


-- | Cite a list of documents
citet, citep :: MonadAuthoring s w m => [String] -> m ()

citet = citationGen "citet"
citep = citationGen "citep"

-- | Cite a single document
citet1, citep1 :: MonadAuthoring s w m => String -> m ()

citet1 = citationGen "citet" . (:[]) -- + ---<===   I am a long man lying
citep1 = citationGen "citep" . (:[]) -- + ---<===   We are long men lying

-- | make a citation to a document(s).
citationGen :: MonadAuthoring s w m => Text.Text -> [String] -> m ()
citationGen cmdName  urls = do
  forM_ urls $ \url -> do
    resolve url
    citedUrlSet %= Set.insert url 
  command1 cmdName $ braces $ raw $ Text.intercalate "," $ map Text.pack urls
