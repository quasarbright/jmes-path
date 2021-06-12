{-# LANGUAGE FlexibleInstances #-}
module JMESPath
    ( 
    -- * Core Types
      Selector()
    , Expr()
    , IsExpr(..)
    -- * Evaluation
    , eval
    , (.?)
    -- * Expr Combinators
    , (|>)
    -- * Selector Combinators
    , (?.)
    , (?..)
    , (?.*)
    , (?!)
    , (?!:)
    , Slice(..)
    , (?!*)
    , (?&)
    , prop
    , multiSelect
    , index
    , slice
    , arrayWild
    , objWild
    , remap
    , select
    ) where

import Data.Text (Text)
import Data.Aeson ( Array, Object, Value(Object, Null, Array) )
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import qualified Data.Vector as Vector
import Data.Vector(Vector)
import Data.Foldable ()
import Control.Monad (join)
import Control.Arrow ((>>>), Arrow (second))
import Data.HashMap.Strict (HashMap)

-- TODO haddoc repls

-- | Represents a Python-like list slice. Slice start stop step
data Slice = MkSlice (Maybe Int) (Maybe Int) (Maybe Int) deriving(Eq, Ord)

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
    = Prop Text
    -- ^ .x
    | MultiSelect [Expr]
    -- ^ .[x,y.z,z[0]]
    | Remap (HashMap Text Expr)
    -- ^ .{firstName: name.first, lastName: name.last}
    | Index Int
    -- ^ [0]
    | Slice Slice
    -- ^ [3:5]
    | ObjectProjection
    -- ^ .*
    -- TODO flatten
    -- TODO function
    -- TODO filter
    deriving(Eq, Ord)

-- | Helper for matching on an object
onObject :: Value -> (Object -> a) -> Maybe a
onObject (Object o) f = Just (f o)
onObject _ _ = Nothing

-- | Helper for matching on an object with a function that outputs a maybe
onObject' :: Value -> (Object -> Maybe a) -> Maybe a
onObject' f v = join (onObject f v)

-- | Helper for matching on an array
onArray :: Value -> (Array -> a) -> Maybe a
onArray (Array v) f = Just (f v)
onArray _ _ = Nothing

-- | Helper for matching on an array with a function that outputs a maybe
onArray' :: Value -> (Array -> Maybe a) -> Maybe a
onArray' f v = join (onArray f v)

-- TODO strictness a la fold' (just $! ?)

-- | Run selectors on a value
selects :: [Selector] -> Value -> Value
selects [] v = v
selects (selector:selectors) v =
    let -- | recur on selectors
        go :: Value -> Value
        go = selects selectors
        -- | recur on selectors for all values, and wrap result in an Array Value
        goProj :: Array -> Value
        goProj = Vector.toList >>> fmap go >>> Vector.fromList >>> Array
    in case selector of
        Prop name -> go $ fromMaybe Null $ v `onObject'` \object -> object Map.!? name
        MultiSelect sss -> go $
            eval v <$> sss
            & filter (/= Null)
            & Vector.fromList
            & Array
        Remap mapping -> go $
            mapping
            & Map.toList
            & fmap (second (eval v))
            & Map.fromList
            & Object
        Index i -> go $ fromMaybe Null $ v `onArray'` \vector -> vector Vector.!? i
        Slice s -> goProj $ fromMaybe mempty $ v `onArray` runSlice s
        ObjectProjection -> goProj $ fromMaybe mempty $ v `onObject` \object -> foldMap Vector.singleton object

-- Maybe you could make lenses/traversals for these! could use function composition with (.)!!
-- Selector composition isn't context-free, but Exprs could certainly be a lens/traversal

-- | Represents a full JMESPath expression/query. Use `Monoid` instance or combinators for pipes
data Expr
    = Selects [Selector]
    -- ^ .x[0].y.z[1]
    | Pipe Expr Expr
    -- ^ [*].x | .y   stops projections: v .? (l |> r) == (v .? l) .? r
    deriving(Eq, Ord)

instance Semigroup Expr where
    Selects ss <> Selects ss' = Selects ss `Pipe` Selects ss'
    Pipe a b <> c = (a <> b) <> c
    a <> Pipe b c = (a <> b) <> c

instance Monoid Expr where
    mempty = Selects mempty

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


-- | Evaluate an expression on a JSON value
infixl 4 `eval`
eval :: Value -> Expr -> Value
eval v = \case
    Selects selectors -> selects selectors v
    Pipe l r -> v `eval` l `eval` r

-- | Runs the selectors on the value
infixl 4 .?
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

-- | slices an object's values (projects)
objWild :: Selector
objWild = ObjectProjection

-- | evaluates exprs on value and maps results to names to create a new object (does not project)
remap :: IsExpr e => [(Text, e)] -> Selector
remap =
    fmap (second toExpr)
    >>> Map.fromList
    >>> Remap

-- | base case for selection list (performs no transformation). Useful for combinator expressions
select :: [Selector]
select = mempty
