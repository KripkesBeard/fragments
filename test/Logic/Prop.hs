module Logic.Prop (tests) where

-- Testing Imports
import Test.Tasty 
    ( testGroup
    , TestTree 
    )
import qualified Test.Tasty.HUnit as H 
    ( testCase
    , (@?=) 
    )


-- Source code imports
import Semantics.Logic.Propositional.Syntax
    ( PropositionalVariable
    , PropositionalFormula (..)
    )
import Semantics.Logic.Propositional.Semantics
  ( Interpretation
  , createInterpretation  
  , evaluateIn
  )



-- | Main test tree containing unit tests and property based tests
-- for propositional logic
tests :: TestTree
tests = testGroup "All tests" [unitTests]

-- Interpretations
inter1 :: Interpretation
inter1 = createInterpretation ["a","b"]
inter2 :: Interpretation
inter2 = createInterpretation []
inter3 :: Interpretation
inter3 = createInterpretation ["a","b","c"]

-- | Unit tests for propositional logic
unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [
    ---- Atomics -----------------------------
    ---- Model 1 -----------------------------
      H.testCase "Atomic 1 model 1" $       --
        evaluateIn atom1 inter1 H.@?= True  --
    , H.testCase "Atomic 2 model 1" $       --
        evaluateIn atom2 inter1 H.@?= True  --
    , H.testCase "Atomic 3 model 1" $       --
        evaluateIn atom3 inter1 H.@?= False --
    ---- Model 2 -----------------------------
    , H.testCase "Atomic 1 model 2" $       --
        evaluateIn atom1 inter2 H.@?= False --
    , H.testCase "Atomic 2 model 2" $       --
        evaluateIn atom2 inter2 H.@?= False --
    , H.testCase "Atomic 3 model 2" $       --
        evaluateIn atom3 inter2 H.@?= False --
    ---- Model 3 -----------------------------
    , H.testCase "Atomic 1 model 3" $       --
        evaluateIn atom1 inter3 H.@?= True  --
    , H.testCase "Atomic 2 model 3" $       --
        evaluateIn atom2 inter3 H.@?= True  --
    , H.testCase "Atomic 3 model 3" $       --
        evaluateIn atom3 inter3 H.@?= True  --
    ------------------------------------------
                                            --
    ---- Negations ---------------------------
    ---- Model 1 -----------------------------
    , H.testCase "Negation 1 model 1" $     --
        evaluateIn neg1 inter1 H.@?= False  --
    , H.testCase "Negation 2 model 1" $     --
        evaluateIn neg2 inter1 H.@?= False  --
    , H.testCase "Negation 3 model 1" $     --
        evaluateIn neg3 inter1 H.@?= True   --
    ---- Model 2 -----------------------------
    , H.testCase "Negation 1 model 2" $     --
        evaluateIn neg1 inter2 H.@?= True   --
    , H.testCase "Negation 2 model 2" $     --
        evaluateIn neg2 inter2 H.@?= True   --
    , H.testCase "Negation 3 model 2" $     --
        evaluateIn neg3 inter2 H.@?= True   --
    ---- Model 3 -----------------------------
    , H.testCase "Negation 1 model 3" $     --
        evaluateIn neg1 inter3 H.@?= False  --
    , H.testCase "Negation 2 model 3" $     --
        evaluateIn neg2 inter3 H.@?= False  --
    , H.testCase "Negation 3 model 3" $     --
        evaluateIn neg3 inter3 H.@?= False  --
    ------------------------------------------
                                            --
    ---- Conjunctions ------------------------
    ---- Model 1 -----------------------------
    , H.testCase "Conjunction 1 model 1" $  --
        evaluateIn con1 inter1 H.@?= True   --
    , H.testCase "Conjunction 2 model 1" $  --
        evaluateIn con2 inter1 H.@?= True   --
    , H.testCase "Conjunction 3 model 1" $  --
        evaluateIn con3 inter1 H.@?= False  --
    , H.testCase "Conjunction 4 model 1" $  --
        evaluateIn con4 inter1 H.@?= True   --
    , H.testCase "Conjunction 5 model 1" $  --
        evaluateIn con5 inter1 H.@?= True   --
    , H.testCase "Conjunction 6 model 1" $  --
        evaluateIn con6 inter1 H.@?= False  --
    , H.testCase "Conjunction 7 model 1" $  --
        evaluateIn con7 inter1 H.@?= False  --
    , H.testCase "Conjunction 8 model 1" $  --
        evaluateIn con8 inter1 H.@?= False  --
    , H.testCase "Conjunction 9 model 1" $  --
        evaluateIn con9 inter1 H.@?= False  --
    ---- Model 2 -----------------------------
    , H.testCase "Conjunction 1 model 2" $  --
        evaluateIn con1 inter2 H.@?= False  --
    , H.testCase "Conjunction 2 model 2" $  --
        evaluateIn con2 inter2 H.@?= False  --
    , H.testCase "Conjunction 3 model 2" $  --
        evaluateIn con3 inter2 H.@?= False  --
    , H.testCase "Conjunction 4 model 2" $  --
        evaluateIn con4 inter2 H.@?= False  --
    , H.testCase "Conjunction 5 model 2" $  --
        evaluateIn con5 inter2 H.@?= False  --
    , H.testCase "Conjunction 6 model 2" $  --
        evaluateIn con6 inter2 H.@?= False  --
    , H.testCase "Conjunction 7 model 2" $  --
        evaluateIn con7 inter2 H.@?= False  --
    , H.testCase "Conjunction 8 model 2" $  --
        evaluateIn con8 inter2 H.@?= False  --
    , H.testCase "Conjunction 9 model 2" $  --
        evaluateIn con9 inter2 H.@?= False  --
    ---- Model 3 -----------------------------
    , H.testCase "Conjunction 1 model 3" $  --
        evaluateIn con1 inter3 H.@?= True   --
    , H.testCase "Conjunction 2 model 3" $  --
        evaluateIn con2 inter3 H.@?= True   --
    , H.testCase "Conjunction 3 model 3" $  --
        evaluateIn con3 inter3 H.@?= True   --
    , H.testCase "Conjunction 4 model 3" $  --
        evaluateIn con4 inter3 H.@?= True   --
    , H.testCase "Conjunction 5 model 3" $  --
        evaluateIn con5 inter3 H.@?= True   --
    , H.testCase "Conjunction 6 model 3" $  --
        evaluateIn con6 inter3 H.@?= True   --
    , H.testCase "Conjunction 7 model 3" $  --
        evaluateIn con7 inter3 H.@?= True   --
    , H.testCase "Conjunction 8 model 3" $  --
        evaluateIn con8 inter3 H.@?= True   --
    , H.testCase "Conjunction 9 model 3" $  --
        evaluateIn con9 inter3 H.@?= True   -- 
    ------------------------------------------
                                            --
    ---- Disjunctions ------------------------
    ---- Model 1 -----------------------------
    , H.testCase "Disjunction 1 model 1" $  --
        evaluateIn dis1 inter1 H.@?= True   --
    , H.testCase "Disjunction 2 model 1" $  --
        evaluateIn dis2 inter1 H.@?= True   --
    , H.testCase "Disjunction 3 model 1" $  --
        evaluateIn dis3 inter1 H.@?= True   --
    , H.testCase "Disjunction 4 model 1" $  --
        evaluateIn dis4 inter1 H.@?= True   --
    , H.testCase "Disjunction 5 model 1" $  --
        evaluateIn dis5 inter1 H.@?= True   --
    , H.testCase "Disjunction 6 model 1" $  --
        evaluateIn dis6 inter1 H.@?= True   --
    , H.testCase "Disjunction 7 model 1" $  --
        evaluateIn dis7 inter1 H.@?= True   --
    , H.testCase "Disjunction 8 model 1" $  --
        evaluateIn dis8 inter1 H.@?= True   --
    , H.testCase "Disjunction 9 model 1" $  --
        evaluateIn dis9 inter1 H.@?= False  --
    ---- Model 2 -----------------------------
    , H.testCase "Disjunction 1 model 2" $  --
        evaluateIn dis1 inter2 H.@?= False  --
    , H.testCase "Disjunction 2 model 2" $  --
        evaluateIn dis2 inter2 H.@?= False  --
    , H.testCase "Disjunction 3 model 2" $  --
        evaluateIn dis3 inter2 H.@?= False  --
    , H.testCase "Disjunction 4 model 2" $  --
        evaluateIn dis4 inter2 H.@?= False  --
    , H.testCase "Disjunction 5 model 2" $  --
        evaluateIn dis5 inter2 H.@?= False  --
    , H.testCase "Disjunction 6 model 2" $  --
        evaluateIn dis6 inter2 H.@?= False  --
    , H.testCase "Disjunction 7 model 2" $  --
        evaluateIn dis7 inter2 H.@?= False  --
    , H.testCase "Disjunction 8 model 2" $  --
        evaluateIn dis8 inter2 H.@?= False  --
    , H.testCase "Disjunction 9 model 2" $  --
        evaluateIn dis9 inter2 H.@?= False  --
    ---- Model 3 -----------------------------
    , H.testCase "Disjunction 1 model 3" $  --
        evaluateIn dis1 inter3 H.@?= True   --
    , H.testCase "Disjunction 2 model 3" $  --
        evaluateIn dis2 inter3 H.@?= True   --
    , H.testCase "Disjunction 3 model 3" $  --
        evaluateIn dis3 inter3 H.@?= True   --
    , H.testCase "Disjunction 4 model 3" $  --
        evaluateIn dis4 inter3 H.@?= True   --
    , H.testCase "Disjunction 5 model 3" $  --
        evaluateIn dis5 inter3 H.@?= True   --
    , H.testCase "Disjunction 6 model 3" $  --
        evaluateIn dis6 inter3 H.@?= True   --
    , H.testCase "Disjunction 7 model 3" $  --
        evaluateIn dis7 inter3 H.@?= True   --
    , H.testCase "Disjunction 8 model 3" $  --
        evaluateIn dis8 inter3 H.@?= True   --
    , H.testCase "Disjunction 9 model 3" $  --
        evaluateIn dis9 inter3 H.@?= True   --
    ------------------------------------------
                                            --
    ---- Implications ------------------------
    ---- Model 1 -----------------------------
    , H.testCase "Implication 1 model 1" $  --
        evaluateIn imp1 inter1 H.@?= True   --
    , H.testCase "Implication 2 model 1" $  --
        evaluateIn imp2 inter1 H.@?= True   --
    , H.testCase "Implication 3 model 1" $  --
        evaluateIn imp3 inter1 H.@?= False  --
    , H.testCase "Implication 4 model 1" $  --
        evaluateIn imp4 inter1 H.@?= True   --
    , H.testCase "Implication 5 model 1" $  --
        evaluateIn imp5 inter1 H.@?= True   --
    , H.testCase "Implication 6 model 1" $  --
        evaluateIn imp6 inter1 H.@?= False  --
    , H.testCase "Implication 7 model 1" $  --
        evaluateIn imp7 inter1 H.@?= True   --
    , H.testCase "Implication 8 model 1" $  --
        evaluateIn imp8 inter1 H.@?= True   --
    , H.testCase "Implication 9 model 1" $  --
        evaluateIn imp9 inter1 H.@?= True   --
    ---- Model 2 -----------------------------
    , H.testCase "Implication 1 model 2" $  --
        evaluateIn imp1 inter2 H.@?= True   --
    , H.testCase "Implication 2 model 2" $  --
        evaluateIn imp2 inter2 H.@?= True   --
    , H.testCase "Implication 3 model 2" $  --
        evaluateIn imp3 inter2 H.@?= True   --
    , H.testCase "Implication 4 model 2" $  --
        evaluateIn imp4 inter2 H.@?= True   --
    , H.testCase "Implication 5 model 2" $  --
        evaluateIn imp5 inter2 H.@?= True   --
    , H.testCase "Implication 6 model 2" $  --
        evaluateIn imp6 inter2 H.@?= True   --
    , H.testCase "Implication 7 model 2" $  --
        evaluateIn imp7 inter2 H.@?= True   --
    , H.testCase "Implication 8 model 2" $  --
        evaluateIn imp8 inter2 H.@?= True   --
    , H.testCase "Implication 9 model 2" $  --
        evaluateIn imp9 inter2 H.@?= True   --
    ---- Model 3 -----------------------------
    , H.testCase "Implication 1 model 3" $  --
        evaluateIn imp1 inter3 H.@?= True   --
    , H.testCase "Implication 2 model 3" $  --
        evaluateIn imp2 inter3 H.@?= True   --
    , H.testCase "Implication 3 model 3" $  --
        evaluateIn imp3 inter3 H.@?= True   --
    , H.testCase "Implication 4 model 3" $  --
        evaluateIn imp4 inter3 H.@?= True   --
    , H.testCase "Implication 5 model 3" $  --
        evaluateIn imp5 inter3 H.@?= True   --
    , H.testCase "Implication 6 model 3" $  --
        evaluateIn imp6 inter3 H.@?= True   --
    , H.testCase "Implication 7 model 3" $  --
        evaluateIn imp7 inter3 H.@?= True   --
    , H.testCase "Implication 8 model 3" $  --
        evaluateIn imp8 inter3 H.@?= True   --
    , H.testCase "Implication 9 model 3" $  --
        evaluateIn imp9 inter3 H.@?= True   --
    ------------------------------------------
    ]
    where
        -- Atomics
        atom1 :: PropositionalFormula
        atom1 = Atomic "a"
        atom2 :: PropositionalFormula
        atom2 = Atomic "b"
        atom3 :: PropositionalFormula
        atom3 = Atomic "c"
        -- Negations
        neg1 :: PropositionalFormula
        neg1 = Negation atom1
        neg2 :: PropositionalFormula
        neg2 = Negation atom2
        neg3 :: PropositionalFormula
        neg3 = Negation atom3
        -- Conjunctions
        con1 :: PropositionalFormula
        con1 = Conjunction atom1 atom1
        con2 :: PropositionalFormula
        con2 = Conjunction atom1 atom2
        con3 :: PropositionalFormula
        con3 = Conjunction atom1 atom3
        con4 :: PropositionalFormula
        con4 = Conjunction atom2 atom1
        con5 :: PropositionalFormula
        con5 = Conjunction atom2 atom2
        con6 :: PropositionalFormula
        con6 = Conjunction atom2 atom3
        con7 :: PropositionalFormula
        con7 = Conjunction atom3 atom1
        con8 :: PropositionalFormula
        con8 = Conjunction atom3 atom2
        con9 :: PropositionalFormula
        con9 = Conjunction atom3 atom3
        -- Disjunctions
        dis1 :: PropositionalFormula
        dis1 = Disjunction atom1 atom1
        dis2 :: PropositionalFormula
        dis2 = Disjunction atom1 atom2
        dis3 :: PropositionalFormula
        dis3 = Disjunction atom1 atom3
        dis4 :: PropositionalFormula
        dis4 = Disjunction atom2 atom1
        dis5 :: PropositionalFormula
        dis5 = Disjunction atom2 atom2
        dis6 :: PropositionalFormula
        dis6 = Disjunction atom2 atom3
        dis7 :: PropositionalFormula
        dis7 = Disjunction atom3 atom1
        dis8 :: PropositionalFormula
        dis8 = Disjunction atom3 atom2
        dis9 :: PropositionalFormula
        dis9 = Disjunction atom3 atom3
        -- Implications
        imp1 :: PropositionalFormula
        imp1 = Implication atom1 atom1
        imp2 :: PropositionalFormula
        imp2 = Implication atom1 atom2
        imp3 :: PropositionalFormula
        imp3 = Implication atom1 atom3
        imp4 :: PropositionalFormula
        imp4 = Implication atom2 atom1
        imp5 :: PropositionalFormula
        imp5 = Implication atom2 atom2
        imp6 :: PropositionalFormula
        imp6 = Implication atom2 atom3
        imp7 :: PropositionalFormula
        imp7 = Implication atom3 atom1
        imp8 :: PropositionalFormula
        imp8 = Implication atom3 atom2
        imp9 :: PropositionalFormula
        imp9 = Implication atom3 atom3