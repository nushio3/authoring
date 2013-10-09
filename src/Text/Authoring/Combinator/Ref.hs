{-# LANGUAGE OverloadedStrings #-}

module Text.Authoring.Combinator.Ref where

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char (isAlphaNum)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text, pack)

import Text.Authoring.Document
import Text.Authoring.State
import Text.Authoring.Label
import Text.Authoring.Combinator.Writer
import Text.Authoring.Combinator.Meta

-- | refer to a label.  
ref :: (MonadState s m, HasAuthorState s, MonadWriter w m, HasDocument w) => Label -> m ()
ref lab = do
  ret <- getReference lab
  command1 "ref" $ raw ret

-- | insert a label.
label :: (MonadState s m, HasAuthorState s, MonadWriter w m, HasDocument w) => Label -> m ()
label lab = do
  ret <- getReference lab
  command1 "label" $ raw ret

-- | an interface for mapping any Label to a unique, LaTeX-safe text.

getReference :: (MonadState s m, HasAuthorState s) => Label -> m Text
getReference lab = do
  map0 <- use labelMap
  case Map.lookup lab map0 of
    Just str -> return str
    Nothing  -> do
      let cands :: [String]
          cands = cand0 : [s ++ [c]| s <- cands, c <- ['0'..'9']]
          cand0 = map modify $ show lab

          modify :: Char -> Char
          modify c
            | isAlphaNum c = c
            | otherwise    = '.'
                             
          takens :: Set.Set Text
          takens = Set.fromList $ Map.elems map0
          
          isTaken x = Set.member (pack x) takens

          freeStr :: Text
          freeStr = pack $ head $ filter (not . isTaken) cands
      labelMap %= (Map.insert lab freeStr)
      return freeStr
