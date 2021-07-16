{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.JMESPath.Core where

import Data.Aeson ( Array, Object, Value(Null, Array, Object, String, Bool, Number) )
import Data.JMESPath.Internal
    ( Expr(..),
      Selector(..),
      runSlice,
      Comparator(Le, Eq, Neq, Gt, Ge, Lt) )
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as Map
import Control.Arrow ((>>>), Arrow (second))
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Control.Monad (join)
import Data.Scientific (Scientific())

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

-- | Helper for matching on a number with a function that outputs a maybe
onNumber :: Value -> (Scientific -> a) -> Maybe a
onNumber (Number n) f = Just (f n)
onNumber _ _ = Nothing

-- | Helper for flattening items. Injects non-arrays as singletons and leaves arrays untouched
toArray :: Value -> Array
toArray (Array vs) = vs
toArray v = [v]

truthy :: Value -> Bool
truthy = \case
  Array [] -> False
  String "" -> False
  Object m -> not (null m)
  Null -> False
  Bool b -> b
  _ -> True

-- | Run selectors on a value
selects :: [Selector] -> Value -> Value
selects [] v = v
selects (selector : selectors) v =
  let go :: Value -> Value
      go = selects selectors
      goProj :: Array -> Value
      goProj = Vector.toList >>> fmap go >>> filter (/= Null) >>> Vector.fromList >>> Array
  in case selector of
        Prop name -> go $! fromMaybe Null $ v `onObject'` \object -> object Map.!? name
        MultiSelect sss ->
          go $!
            eval v <$> sss
              & filter (/= Null)
              & Vector.fromList
              & Array
        Remap mapping ->
          go $!
            mapping
              & Map.toList
              & fmap (second (eval v))
              & Map.fromList
              & Object
        Index i -> go $! fromMaybe Null $ v `onArray'` \vector -> vector Vector.!? i
        Slice s -> fromMaybe Null $ v `onArray` \vector -> goProj $! runSlice s vector
        Flatten -> fromMaybe Null $ v `onArray` \vector -> goProj $! foldMap toArray vector
        ObjectProjection -> fromMaybe Null $ v `onObject` \object -> goProj $! foldMap Vector.singleton object
        Literal v' -> go $! v'
        Or l r ->
          let l' = eval v l
              r' = eval v r
          in go $! if truthy l'
            then l'
            else r'
        And l r ->
          let l' = eval v l
              r' = eval v r
          in go $! if not (truthy l')
            then l'
            else r'
        Not e -> go $! Bool (not (truthy (eval v e)))
        Filter e -> fromMaybe Null $ v `onArray` \vector -> goProj $! Vector.filter p vector
          where p ele = truthy (eval ele e)
        Comparison l Eq r -> go $! Bool (l == r)
        Comparison l Neq r -> go $! Bool (l /= r)
        Comparison l cmp r ->
          let cmpFn :: (Scientific -> Scientific -> Bool)
              cmpFn = case cmp of
                  Eq -> (==)
                  Neq -> (/=)
                  Gt -> (>)
                  Ge -> (>=)
                  Lt -> (<)
                  Le -> (<=)
          in fromMaybe Null $ do
            l' <- eval v l `onNumber` id
            r' <- eval v r `onNumber` id
            pure (Bool (l' `cmpFn` r'))

-- Maybe you could make lenses/traversals for these! could use function composition with (.)!!
-- Selector composition isn't context-free, but Exprs could certainly be a lens/traversal

-- | Evaluate an expression on a JSON value
infixl 4 `eval`

-- | Evaluate an expression 
eval :: Value -> Expr -> Value
eval v = \case
  Selects selectors -> selects selectors v
  Pipe l r -> v `eval` l `eval` r
