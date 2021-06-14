{-# LANGUAGE FlexibleInstances #-}

module Data.JMESPath
  ( -- * Core Types
    Selector (),
    Expr (),
    IsExpr (..),

    -- * Evaluation
    eval,
    (.?),

    -- * Expr Combinators
    (|>),

    -- * Selector Combinators
    (?.),
    (?..),
    (?.*),
    (?!),
    (?!:),
    Slice (..),
    (?!*),
    (?&),
    prop,
    multiSelect,
    index,
    slice,
    arrayWild,
    listProj,
    flatten,
    objWild,
    objProj,
    remap,
    select,
  )
where

import Control.Arrow (Arrow (second), (>>>))
import Data.Aeson (Value)
import Data.Foldable ()
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import Data.JMESPath.Internal
    ( Selector(..), Slice(..), IsExpr(..), Expr(Pipe) )
import Data.JMESPath.Core ( eval )

-- TODO haddoc repls

-- | Runs the selectors on the value
infixl 4 .?

-- | Infix alias for `eval`
(.?) :: IsExpr e => Value -> e -> Value
(.?) v = eval v . toExpr

infixl 5 |>

-- | Pipes result of left expression into right selectors. Alias for `(.?)`
(|>) :: (IsExpr e, IsExpr e') => e -> e' -> Expr
l |> r = Pipe (toExpr l) (toExpr r)

-- combinators for building `Selector` lists

infixl 6 ?., ?.., ?!, ?!:, ?&, ?.*

-- | Field selector
(?.) :: [Selector] -> Text -> [Selector]
s ?. f = s <> [Prop f]

-- | Multi selector (does not project)
(?..) :: IsExpr e => [Selector] -> [e] -> [Selector]
s ?.. es = s <> [MultiSelect (toExpr <$> es)]

-- | Object projection
(?.*) :: [Selector] -> Selector -> [Selector]
ss ?.* s = ss ?& objWild ?& s

-- | Index selector
(?!) :: [Selector] -> Int -> [Selector]
s ?! i = s <> [Index i]

-- | Slice selector
(?!:) :: [Selector] -> Slice -> [Selector]
s ?!: sl = s <> [Slice sl]

-- | Array projection
(?!*) :: [Selector] -> Selector -> [Selector]
ss ?!* s = ss ?& arrayWild ?& s

-- | Add a selector to the list
(?&) :: [Selector] -> Selector -> [Selector]
ss ?& s = ss <> [s]

-- | Field selector
prop :: Text -> Selector
prop = Prop

-- | Multi selector (does not project)
multiSelect :: IsExpr e => [e] -> Selector
multiSelect = MultiSelect . fmap toExpr

-- | Index selector
index :: Int -> Selector
index = Index

-- | Slice selector
slice :: Maybe Int -> Maybe Int -> Maybe Int -> Selector
slice start stop end = Slice (MkSlice start stop end)

-- | slices the whole array (projects)
arrayWild :: Selector
arrayWild = Slice (MkSlice Nothing Nothing Nothing)

-- | alias for `arrayWild`
listProj :: Selector
listProj = arrayWild

-- | like `arrayWild`, but flattens 
flatten :: Selector
flatten = Flatten

-- | slices an object's values (projects)
objWild :: Selector
objWild = ObjectProjection

-- | selects all object values (projects)
objProj :: Selector
objProj = ObjectProjection

-- | evaluates exprs on value and maps results to names to create a new object (does not project)
remap :: IsExpr e => [(Text, e)] -> Selector
remap =
  fmap (second toExpr)
    >>> Map.fromList
    >>> Remap

-- | base case for selection list (performs no transformation). Useful for combinator expressions
select :: [Selector]
select = mempty
