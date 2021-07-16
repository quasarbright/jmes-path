{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
import Test.Hspec

import Data.Aeson
import Data.JMESPath

pass :: Expectation
pass = True `shouldBe` True

main :: IO ()
main = hspec $ do
    describe "prop select" $ do
        it "works when field is present" $ do
            Object ("a" .= True) .? prop "a" `shouldBe` Bool True
            Object ("a" .= True <> "b" .= False) .? prop "a" `shouldBe` Bool True
            Object ("a" .= True <> "b" .= False <> "c" .= Number 9) .? prop "a" `shouldBe` Bool True
        it "handles absent field" $ do
            Bool True .? prop "a" `shouldBe` Null
            Bool False .? prop "a" `shouldBe` Null
            Null .? prop "a" `shouldBe` Null
            Number 3 .? prop "a" `shouldBe` Null
            String "a" .? prop "a" `shouldBe` Null
            Array [] .? prop "a" `shouldBe` Null
            Array [Object ("a" .= Bool True)] .? prop "a" `shouldBe` Null
        it "chains" $ do
            Object ("a" .= Object ("b" .= Object ("c" .= Bool True))) .? select ?. "a" ?. "b" ?. "c" `shouldBe` Bool True
    describe "index select" $ do
        it "works when item is present" $ do
            Array [Bool True] .? index 0 `shouldBe` Bool True
            Array [Bool True, Bool False, Number 9] .? index 0 `shouldBe` Bool True
            Array [Bool True, Bool False, Number 9] .? index 1 `shouldBe` Bool False
            Array [Bool True, Bool False, Number 9] .? index 2 `shouldBe` Number 9
        it "works when item is not present " $ do
            Array [] .? index 0 `shouldBe` Null
            Object mempty .? index 0 `shouldBe` Null
            Bool True .? index 0 `shouldBe` Null
            Bool False .? index 0 `shouldBe` Null
            Number 0 .? index 0 `shouldBe` Null
            String "a" .? index 0 `shouldBe` Null
            Null .? index 0 `shouldBe` Null
            Array [Bool True] .? index (-1) `shouldBe` Null
            Array [Bool True] .? index 1 `shouldBe` Null
        it "chains" $ do
            Array [Array [Array [Bool True]]] .? select ?! 0 ?! 0 ?! 0 `shouldBe` Bool True
    describe "multi select" $ do
        it "works when all succeed" $ do
            Object ("a" .= Number 1 <> "b" .= Number 2 <> "c" .= Number 3) .? multiSelect [prop "a", prop "b"] `shouldBe` Array [Number 1, Number 2]
        it "works when none succeed" $ do
            Object mempty .? multiSelect [prop "a", prop "b"] `shouldBe` Array []
        it "works when some succeed" $ do
            Object ("a" .= Number 1 <> "b" .= Number 2) .? multiSelect [prop "a", prop "c", prop "b"] `shouldBe` Array [Number 1, Number 2]
        it "doesn't project" $ do
            Object ("a" .= Array [Number 1] <> "b" .= Array [Number 2]) .? select ?& multiSelect [prop "a", prop "b"] ?! 0 `shouldBe` Array [Number 1]
    describe "list projection" $ do
        it "works on arrays" $ do
            Array [Object ("a" .= Number 1 <> "b" .= Number 3), Object ("a" .= Number 2)] .? select ?& arrayWild ?. "a" `shouldBe` Array [Number 1, Number 2]
        it "works on some fails" $ do
            Array [Object ("a" .= Number 1 <> "b" .= Number 3), Object ("d" .= Number 2), Object ("a" .= Number 4)]
                .? select ?& arrayWild ?. "a"
                `shouldBe` Array [Number 1, Number 4]
        it "works on empty" $ do
            Array [] .? select ?& arrayWild ?. "a" `shouldBe` Array []
        it "copies lists" $ do
            Array [Number 1, Number 2] .? select ?& arrayWild `shouldBe` Array [Number 1, Number 2]
        it "works on non-lists" $ do
            Null .? select ?& arrayWild `shouldBe` Null
            Bool True .? select ?& arrayWild `shouldBe` Null
            Bool False .? select ?& arrayWild `shouldBe` Null
            Number 0 .? select ?& arrayWild `shouldBe` Null
            String "a" .? select ?& arrayWild `shouldBe` Null
            Object ("a" .= Array [Number 0]) .? select ?& arrayWild `shouldBe` Null
        it "chains" $ do
            let v = Array [Object ("e" .= Array [Object ("a" .= Number 1), Object ("a" .= Number 2)]), Object ("e" .= Array [Object ("a" .= Number 3)])]
                v' = Array [Array [Number 1, Number 2], Array [Number 3]]
            v .? select ?& arrayWild ?. "e" ?& arrayWild ?. "a" `shouldBe` v'
    describe "object projection" $ do
        it "works on objects" $ do
            Object ("a" .= Array [Number 1, Number 2] <> "b" .= Array [Number 3, Number 4])
                .? select ?& objWild ?! 0
                `shouldBe` Array [Number 1, Number 3]
        it "works on some fails" $ do
            Object ("a" .= Array [Number 1] <> "b" .= Number 2 <> "c" .= Array [Number 3])
                .? select ?& objWild ?! 0
                `shouldBe` Array [Number 1, Number 3]
        it "works on all fails" $ do
            Object ("a" .= Number 0 <> "b" .= Number 1 <> "c" .= Number 2)
                .? select ?& objWild ?! 0
                `shouldBe` Array []
        it "works on empty" $ do
            Object mempty .? select ?& objWild ?! 0 `shouldBe` Array []
        it "works on non-objects" $ do
            Null .? select ?& objWild ?! 0 `shouldBe` Null
            Bool True .? select ?& objWild ?! 0 `shouldBe` Null
            Bool False .? select ?& objWild ?! 0 `shouldBe` Null
            Number 0 .? select ?& objWild ?! 0 `shouldBe` Null
            String "a" .? select ?& objWild ?! 0 `shouldBe` Null
            Array [Number 0] .? select ?& objWild ?! 0 `shouldBe` Null
        it "copies" $ do
            Object ("a" .= Number 0 <> "b" .= Number 1 <> "c" .= Number 2) .? select ?& objWild `shouldBe` Array [Number 0, Number 1, Number 2]
        it "chains" $ do
            let v = Object ("a" .= Array [Object ("b" .= Array [Number 1])] <> "b" .= Array [Object ("c" .= Array [Number 2] <> "d" .= Array [Number 3, Number 4])])
                v' = Array [Array [Number 1], Array [Number 3, Number 2]] -- the order is hash-based, not based on insertion order
            v .? select ?& objWild ?! 0 ?& objWild ?! 0 `shouldBe` v'
    describe "list flatten" $ do
        it "flattens" $ do
            -- taken from tutorial
            let -- [[0,1],2,[3],4,[5,[6,7]]]
                v = Array [Array [Number 0, Number 1], Number 2, Array [Number 3], Number 4, Array [Number 5, Array [Number 6, Number 7]]]
                -- [0,1,2,3,4,5,[6,7]]
                v' = Array [Number 0, Number 1, Number 2, Number 3, Number 4, Number 5, Array [Number 6, Number 7]]
            v .? select ?& flatten `shouldBe` v'
        it "projects" $ do
            let -- [[0, 1],2,[3],4,[5, [6, 7], 8, [9]]
                v = Array [Array [Number 0, Number 1], Number 2, Array [Number 3], Number 4, Array [Number 5, Array [Number 6, Number 7], Number 8, Array [Number 9]]]
                v' = Array [Number 6, Number 9]
            v .? select ?& flatten ?! 0 `shouldBe` v'
        it "works on non-arrays" $ do
            Null .? select ?& flatten `shouldBe` Null
            Bool True .? select ?& flatten `shouldBe` Null
            Bool False .? select ?& flatten `shouldBe` Null
            Number 1 .? select ?& flatten `shouldBe` Null
            String "a" .? select ?& flatten `shouldBe` Null
            Object ("a" .= Number 1) .? select ?& flatten `shouldBe` Null
        it "works on empty"  $ do
            Array [] .? select ?& flatten `shouldBe` Array []
    describe "remap" $ do
        it "works on objects" $ do
            Object ("a" .= Number 1) .? select ?& remap [("b", prop "a")] `shouldBe` Object ("b" .= Number 1)
        it "works on arrays" $ do
            Array [Number 0, String "a", Bool True]
                .? select ?& remap [("b", index 0), ("a", index 2), ("c", index 1)]
                `shouldBe` Object ("a" .= Bool True <> "b" .= Number 0 <> "c" .= String "a")
        it "works with fails" $ do
            Object ("a" .= Number 1) .? select ?& remap [("b", prop "c"), ("foo", prop "a"), ("c", index 0)] `shouldBe` Object ("foo" .= Number 1 <> "b" .= Null <> "c" .= Null)
        it "allows a selector to be reused" $ do
            Object ("a" .= Number 1) .? select ?& remap [("a", prop "a"), ("b", prop "a")] `shouldBe` Object ("a" .= Number 1 <> "b" .= Number 1)
    describe "literal" $ do
        it "works on all types" $ do
            let v = Array [Number 0, Bool True]
            Null .? select ?& literal v `shouldBe` v
            Number 0 .? select ?& literal v `shouldBe` v
            String "a" .? select ?& literal v `shouldBe` v
            Bool True .? select ?& literal v `shouldBe` v
            Bool False .? select ?& literal v `shouldBe` v
            Array [Number 0] .? select ?& literal v `shouldBe` v
            Array [] .? select ?& literal v `shouldBe` v
            Object ("a" .= Number 0) .? select ?& literal v `shouldBe` v
            Object mempty .? select ?& literal v `shouldBe` v
        it "works after a selector" $ do
            let v = Array [Number 0, Bool True]
            Array [Number 0] .? select ?! 0 ?& literal v `shouldBe` v
            Object ("a" .= Number 0) .? select ?! 0 ?& literal v `shouldBe`v
        it "works after a projection" $ do
            let v = Array [Number 0]
            Array [Number 0, Number 1] .? select ?& arrayWild ?& literal v `shouldBe` Array [v, v]
        it "works after a failure" $ do
            Number 1 .? select ?! 0 ?& literal (String "a") `shouldBe` String "a"
        it "chains with other selectors after it" $ do
            pass -- TODO
    describe "truthiness" $ do
        it "works on singles" $ do
            Array [Null] .? toBool (index 0) `shouldBe` Bool False
            Array [Array []] .? toBool (index 0) `shouldBe` Bool False
            Array [String ""] .? toBool (index 0) `shouldBe` Bool False
            Array [Object mempty] .? toBool (index 0) `shouldBe` Bool False
            Array [Bool False] .? toBool (index 0) `shouldBe` Bool False
            Array [Bool True] .? toBool (index 0) `shouldBe` Bool True
            Array [Number 0] .? toBool (index 0) `shouldBe` Bool True
            Array [Array [Null]] .? toBool (index 0) `shouldBe` Bool True
            Array [Object ("" .= Null)] .? toBool (index 0) `shouldBe` Bool True
        it "is idempotent" $ do
            pass -- TODO property test v .? toBool ?& toBool == v .? toBool
    describe "or" $ do
        it "works on singles" $ do
            let e = prop "a" ?|| prop "b"
                v a b = Object ("a" .= a <> "b" .= b)
                q a b = v a b .? e
            q (Number 1) (Number 2) `shouldBe` Number 1
            q (Number 1) Null `shouldBe` Number 1
            q Null (Number 1) `shouldBe` Number 1
            q Null Null `shouldBe` Null
            q Null (String "") `shouldBe` String ""
            q (String "") Null `shouldBe` Null
        it "chains" $ do
            Array [Array [Number 1], String ""] .? select ?& index 0 ?|| index 1 ?! 0 `shouldBe` Number 1
            Array [String "",Array [Number 1]] .? select ?& index 0 ?|| index 1 ?! 0 `shouldBe` Number 1
        it "associates" $ do
            Array [Number 1, Null, Null] .? select ?& (index 0 ?|| index 1) ?|| index 2 `shouldBe` Number 1
            Array [Null, Number 1, Null] .? select ?& (index 0 ?|| index 1) ?|| index 2 `shouldBe` Number 1
            Array [Null, Null, Number 1] .? select ?& (index 0 ?|| index 1) ?|| index 2 `shouldBe` Number 1
            Array [Number 1, Null, Null] .? select ?& index 0 ?|| (index 1 ?|| index 2) `shouldBe` Number 1
            Array [Null, Number 1, Null] .? select ?& index 0 ?|| (index 1 ?|| index 2) `shouldBe` Number 1
            Array [Null, Null, Number 1] .? select ?& index 0 ?|| (index 1 ?|| index 2) `shouldBe` Number 1
            Array [Number 1, Number 2, Null] .? select ?& (index 0 ?|| index 1) ?|| index 2 `shouldBe` Number 1
            Array [Null, Number 1, Number 2] .? select ?& (index 0 ?|| index 1) ?|| index 2 `shouldBe` Number 1
            Array [Number 1, Null, Number 2] .? select ?& (index 0 ?|| index 1) ?|| index 2 `shouldBe` Number 1
            Array [Number 1, Number 2, Null] .? select ?& index 0 ?|| (index 1 ?|| index 2) `shouldBe` Number 1
            Array [Null, Number 1, Number 2] .? select ?& index 0 ?|| (index 1 ?|| index 2) `shouldBe` Number 1
            Array [Number 1, Number 2, Null] .? select ?& index 0 ?|| (index 1 ?|| index 2) `shouldBe` Number 1
    describe "and" $ do
        it "works on singles" $ do
            let e = prop "a" ?&& prop "b"
                v a b = Object ("a" .= a <> "b" .= b)
                q a b = v a b .? e
            q (Number 1) (Number 2) `shouldBe` Number 2
            q Null (Number 1) `shouldBe` Null
            q (Number 1) Null `shouldBe` Null
            q Null Null `shouldBe` Null
            q (String "") (Number 1) `shouldBe` String ""
        it "chains" $ do
            Array [Array [Number 1], Array [Number 2]] .? select ?& index 0 ?&& index 1 ?! 0 `shouldBe` Number 2
            Array [Array [Number 2], Array [Number 1]] .? select ?& index 0 ?&& index 1 ?! 0 `shouldBe` Number 1
        it "associates" $ do
            Array [Null, Number 1, Number 2] .? select ?& (index 0 ?&& index 1) ?&& index 2 `shouldBe` Null
            Array [Number 0, Number 1, Number 2] .? select ?& (index 0 ?&& index 1) ?&& index 2 `shouldBe` Number 2
            Array [Number 0, Number 1, Number 2] .? select ?& index 0 ?&& (index 1 ?&& index 2) `shouldBe` Number 2
            Array [Number 0, Number 1, Null] .? select ?& index 0 ?&& (index 1 ?&& index 2) `shouldBe` Null
            Array [Number 0, Number 1, Null] .? select ?& (index 0 ?&& index 1) ?&& index 2 `shouldBe` Null
    describe "filter" $ do
        it "works on singles" $ do
            Array [Array [Number 0], Array [Number 1, Number 2]] .? jfilter (index 0)
        it "projects" $ do
            pass
        it "handles the check failing" $ do
            pass
        -- TODO test cmp
    describe "pipes" $ do
        it "stops projections" $ do
            let v = Array [Object ("a" .= Array [Number 1, Number 2]), Object ("a" .= Array [Number 3, Number 4])]
                vProj = Array [Number 1, Number 3]
                vPipe = Array [Number 1, Number 2]
            v .? select ?& arrayWild ?. "a" ?! 0 `shouldBe` vProj
            v .? select ?& arrayWild ?. "a" |> select ?! 0 `shouldBe` vPipe
        it "has identity" $ do
            pass -- TODO property test id category law
        it "compose associatively" $ do
            pass -- TODO property test assoc category law
        it "just makes a new eval (pipe law)" $ do
            pass -- TODO property test pipe law: v .? (a |> b) == (v .? a) .? b
