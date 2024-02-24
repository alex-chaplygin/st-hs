module Types (Object(..)) where

import Data.List
import Data.Array
import qualified Data.Vector as V

data Object = SYMBOL String
  | NUM Int
  | LIST [Object]
  deriving (Eq)

instance Show Object where
  show (SYMBOL s) = s
  show (NUM i) = (show i)
  show (LIST []) = "NIL"
  show (LIST l) = "(" ++ (concat $ intersperse " " $ map show l) ++ ")"
