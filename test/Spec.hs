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
    describe "pipe law" $ do
        it "is obeyed" $ do
            -- TODO property test pipe law: v .? (a |> b) == (v .? a) .? b
            pass