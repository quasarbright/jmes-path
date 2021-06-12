{-# LANGUAGE FlexibleInstances #-}
module Data.JMESPath.Internal where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)

-- | Represents a Python-like list slice. Slice start stop step
data Slice = MkSlice (Maybe Int) (Maybe Int) (Maybe Int) deriving (Eq, Ord)

-- TODO finish and test edge cases
runSlice :: Slice -> Vector a -> Vector a
runSlice (MkSlice start stop _) v = Vector.slice start' len v
  where
    start' = fromMaybe 0 start
    stop' = fromMaybe (Vector.length v) stop
    len = stop' - start'
    -- step' = fromMaybe 1 step

-- TODO arbitrary selectors in multi-select

-- | Represents query selectors. Field accesses, indexing, etc. Also used for projections
data Selector
  = -- | .x
    Prop Text
  | -- | .[x,y.z,z[0]]
    MultiSelect [Expr]
  | -- | .{firstName: name.first, lastName: name.last}
    Remap (HashMap Text Expr)
  | -- | [0]
    Index Int
  | -- | [3:5]
    Slice Slice
  | -- | .*
    -- TODO flatten
    -- TODO function
    -- TODO filter
    ObjectProjection
  deriving (Eq, Ord)

-- | Represents a full JMESPath expression/query. Use `Monoid` instance or combinators for pipes
data Expr
  = -- | .x[0].y.z[1]
    Selects [Selector]
  | -- | [*].x | .y   stops projections: v .? (l |> r) == (v .? l) .? r
    Pipe Expr Expr
  deriving (Eq, Ord)

instance Semigroup Expr where
  Selects ss <> Selects ss' = Selects ss `Pipe` Selects ss'
  Pipe a b <> c = (a <> b) <> c
  a <> Pipe b c = (a <> b) <> c

instance Monoid Expr where
  mempty = Selects mempty

-- | Used for overloads
class IsExpr e where
  toExpr :: e -> Expr

instance IsExpr Expr where
  toExpr = id

instance IsExpr [Selector] where
  toExpr = Selects

instance IsExpr (Vector Selector) where
  toExpr = toExpr . Vector.toList

instance IsExpr Selector where
  toExpr s = toExpr [s]