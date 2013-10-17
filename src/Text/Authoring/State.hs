{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Authoring.State where

import           Control.Lens
import           Control.Lens.TH
import           Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Text.CSL.Input.Identifier.Internal as Citation

import           Text.Authoring.Label

-- | The record type of everything you need to know
--   to write a paper.

data AuthorState
  = AuthorState
  { _labelMap :: Map.Map Label Text
  , _citationDB :: Citation.Database
  , _citedUrlSet :: Set.Set String
  }

makeClassy ''AuthorState

instance Citation.HasDatabase AuthorState where
  database = citationDB
  
instance Default AuthorState where
  def = AuthorState def def def
