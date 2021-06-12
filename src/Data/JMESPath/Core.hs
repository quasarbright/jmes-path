module Data.JMESPath.Core where

import Data.Aeson ( Array, Object, Value(Null, Array, Object) )
import Data.JMESPath.Internal ( Expr(..), Selector(..), runSlice )
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as Map
import Control.Arrow ((>>>), Arrow (second))
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Control.Monad (join)

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
selects (selector : selectors) v =
  let go :: Value -> Value
      go = selects selectors

      goProj :: Array -> Value
      goProj = Vector.toList >>> fmap go >>> Vector.fromList >>> Array
   in case selector of
        Prop name -> go $ fromMaybe Null $ v `onObject'` \object -> object Map.!? name
        MultiSelect sss ->
          go $
            eval v <$> sss
              & filter (/= Null)
              & Vector.fromList
              & Array
        Remap mapping ->
          go $
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

-- | Evaluate an expression on a JSON value
infixl 4 `eval`

eval :: Value -> Expr -> Value
eval v = \case
  Selects selectors -> selects selectors v
  Pipe l r -> v `eval` l `eval` r