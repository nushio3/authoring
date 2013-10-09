module Text.Authoring.Label 
       ( Label(..),(<>),(./))
       where

import           Data.Monoid
import           Data.String (IsString(..))
import           Data.Typeable
import           Data.Text (pack)
import           Text.LaTeX (raw)
import           Text.LaTeX.Base.Texy

-- | 'Label's are used to create a unique reference label
--   within a paper.
data Label 
  = FromType !TypeRep 
  | FromString !String
  | Ap !Label !Label
  | Empty
  deriving (Eq, Ord)

instance Monoid Label where
  mempty = Empty
  mappend Empty x = x
  mappend x Empty = x
  mappend x y = Ap x y

instance IsString Label where
  fromString = FromString

instance Show Label where
  show (FromType r) = show r
  show (FromString s) = s
  show (Ap a b) = show a ++ ":" ++ show b

instance Texy Label where
  texy = raw . pack . show

infixl 1 ./

-- | Create a slightly different version of the label.
(./) :: Show a => Label -> a -> Label
l ./ x = Ap l (FromString $ show x)
