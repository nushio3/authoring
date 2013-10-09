{-# LANGUAGE OverloadedStrings #-}

module Text.Authoring.Combinator.Meta where

import Control.Monad.Writer
import Data.Text (Text)

import Text.Authoring.Document
import Text.Authoring.Combinator.Writer


-- | wrap the argument @x@ using curly braces "{x}"

braces :: (MonadWriter t m, HasDocument t) => m () -> m ()
braces con = do
  raw "{"
  con
  raw "}"

  
-- | command1 x con = "\x{con}"

command1 :: (MonadWriter t m, HasDocument t) =>  Text -> m () -> m ()
command1 x con = do
  raw "\\"  
  raw x
  raw "{"
  con
  raw "}"
       
    
-- | environment x con = "\begin{x}con\end{x}"    
  
environment :: (MonadWriter t m, HasDocument t) =>  Text -> m () -> m ()
environment x con = do
  raw "\\begin{"  
  raw x
  raw "}\n"  
  con
  raw "\\end{"  
  raw x
  raw "}\n"  

