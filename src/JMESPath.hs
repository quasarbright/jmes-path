module JMESPath
    ( 
    -- * Core Types
      Selector()
    , Expr()
    -- * Evaluation
    , eval
    , (.?)
    -- * Builder Combinators
    -- ** Expr Combinators
    , (|>)
    , (^|>)
    -- ** Selector Combinators
    , (?.)
    , (?..)
    , (?!)
    , (?:)
    , Slice(..)
    , arrayWild
    , objWild
    , select
    ) where

import Data.Text (Text)
import Data.Aeson ( Array, Object, Value(Object, Null, Array) )
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Function ((&))
import qualified Data.Vector as Vector
import Data.Vector(Vector)
import Data.Foldable ()
import Control.Monad (join)
import Control.Arrow ((>>>))

-- | Represents a Python-like list slice. Slice start stop step
data Slice = MkSlice (Maybe Int) (Maybe Int) (Maybe Int) deriving(Eq, Ord)

-- TODO finish and test edge cases
slice :: Slice -> Vector a -> Vector a
slice (MkSlice start stop _) v = Vector.slice start' len v
    where
        start' = fromMaybe 0 start
        stop' = fromMaybe (Vector.length v) stop
        len = stop' - start'
        -- step' = fromMaybe 1 step

-- | Represents query selectors. Field accesses, indexing, etc. Also used for projections
data Selector
    = Prop Text
    -- ^ .x
    | Props [Text]
    -- ^ .[x,y,z]
    | Index Int
    -- ^ [0]
    | Slice Slice
    -- ^ [3:5]
    | ObjectProjection
    -- ^ .*
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
        Props names -> go $ fromMaybe Null $ v `onObject` \object ->
            mapMaybe (`Map.lookup` object) names
            & Vector.fromList
            & Array
        Index i -> go $ fromMaybe Null $ v `onArray'` \vector -> vector Vector.!? i
        Slice s -> goProj $ fromMaybe mempty $ v `onArray` slice s
        ObjectProjection -> goProj $ fromMaybe mempty $ v `onObject` \object -> foldMap Vector.singleton object

-- Maybe you could make lenses/traversals for these! could use function composition with (.)!!

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

-- | Evaluate an expression on a JSON value
eval :: Value -> Expr -> Value
eval v = \case
    Selects selectors -> selects selectors v
    Pipe l r -> v .? l .? r

-- | infix alias for `eval`
infixl 4 .?
(.?) :: Value -> Expr -> Value
(.?) = eval

-- combinators for building `Expr`s

infixl 5 |>, ^|>
-- | Pipes result of left expression into right selectors
(|>) :: Expr -> [Selector] -> Expr
e |> ss = e <> Selects ss

-- | Pipes result of left selectors into right selectors
(^|>) :: [Selector] -> [Selector] -> Expr
ss ^|> ss' = Selects ss <> Selects ss'

-- combinators for building `Selector` lists

infixl 6 ?., ?.., ?!, ?:

-- | Field accessor
(?.) :: [Selector] -> Text -> [Selector]
s ?. f = s <> [Prop f]

-- | Multi-field accessor
(?..) :: [Selector] -> [Text] -> [Selector]
s ?.. fs = s <> [Props fs]

-- | Index accessor
(?!) :: [Selector] -> Int -> [Selector]
s ?! i = s <> [Index i]

-- | Slice accessor
(?:) :: [Selector] -> Slice -> [Selector]
s ?: sl = s <> [Slice sl]

-- | slices the whole array (used for list projections)
arrayWild :: Selector
arrayWild = Slice (MkSlice Nothing Nothing Nothing)

-- | slices an object's values (used for object projections)
objWild :: Selector
objWild = ObjectProjection

-- | base case for selections (identity selector)
select :: [Selector]
select = mempty
