module Logic.Quant (tests) where

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
import Semantics.Logic.Quantificational.Syntax
    ( QuantificationalTerm
    , QuantificationalFormula (..)
    )
import Semantics.Logic.Quantificational.Semantics 
    ( Domain
    , createDomain
    , RelationalTuples 
    , createRelationalTuple
    , Relations
    , createRelations
    , Interpretation
    , createInterpretation
    , evaluateIn
    )



-- | Main test tree containing unit tests and property based tests
-- for quantificational logic
tests :: TestTree
tests = testGroup "All tests" [unitTests]

-- Model creation

d :: Domain
d = createDomain ["Evil Cooper", "Laura", "BOB", "Cooper", "Annie", "Windom", "Gordon"]

r :: Relations
r = createRelations [ ("Dead", createRelationalTuple [["Laura"], ["Windom"]])
                    , ("Alive", createRelationalTuple [["BOB"], ["Cooper"], ["Evil Cooper"], ["Gordon"], ["Annie"]])
                    , ("InTwinPeaks", createRelationalTuple [["Evil Cooper"], ["BOB"], ["Gordon"]])
                    , ("InTheBlackLodge", createRelationalTuple [["Laura"], ["Annie"], ["Windom"], ["Cooper"]])
                    , ("Killed", createRelationalTuple [["BOB", "Laura"]])
                    , ("EatsGarmonbozia", createRelationalTuple [["Evil Cooper"], ["BOB"]])
                    , ("Loves", createRelationalTuple [["Cooper", "Annie"], ["Annie", "Cooper"]])]

i :: Interpretation
i = createInterpretation d r

-- | Unit tests for quantificational logic
unitTests :: TestTree 
unitTests = testGroup "Unit tests" 
    [
    ---- Predicates ---------------------
      H.testCase "Predicate 1" $       --
        evaluateIn pred1 i H.@?= True  --
    , H.testCase "Predicate 2" $       --
        evaluateIn pred2 i H.@?= True  --
    , H.testCase "Predicate 3" $       --
        evaluateIn pred3 i H.@?= False --
    -------------------------------------
                                       --
    ---- Equalities ---------------------
    , H.testCase "Equality 1" $        --
        evaluateIn equ1 i H.@?= True   --
    , H.testCase "Equality 2" $        --
        evaluateIn equ2 i H.@?= False  --
    , H.testCase "Equality 3" $        --
        evaluateIn equ3 i H.@?= False  --
    , H.testCase "Equality 3" $        --
        evaluateIn equ3 i H.@?= False  --
    -------------------------------------
                                       --
    ---- Negations ---------------------- 
    , H.testCase "Negation 1" $        --
        evaluateIn neg1 i H.@?= False  --
    , H.testCase "Negation 2" $        --
        evaluateIn neg2 i H.@?= False  --
    , H.testCase "Negation 3" $        --
        evaluateIn neg3 i H.@?= True   --
    -------------------------------------
                                       --
    ---- Conjunctions -------------------
    , H.testCase "Conjunction 1" $     --
        evaluateIn con1 i H.@?= True   --
    , H.testCase "Conjunction 2" $     --
        evaluateIn con2 i H.@?= True   --
    , H.testCase "Conjunction 3" $     --
        evaluateIn con3 i H.@?= False  --
    , H.testCase "Conjunction 4" $     --
        evaluateIn con4 i H.@?= True   --
    , H.testCase "Conjunction 5" $     --
        evaluateIn con5 i H.@?= True   --
    , H.testCase "Conjunction 6" $     --
        evaluateIn con6 i H.@?= False  --
    , H.testCase "Conjunction 7" $     --
        evaluateIn con7 i H.@?= False  --
    , H.testCase "Conjunction 8" $     --
        evaluateIn con8 i H.@?= False  --
    , H.testCase "Conjunction 9" $     --
        evaluateIn con9 i H.@?= False  --
    -------------------------------------
                                       --
    ---- Disjunctions -------------------
    , H.testCase "Disjunction 1" $     --
        evaluateIn dis1 i H.@?= True   --
    , H.testCase "Disjunction 2" $     --
        evaluateIn dis2 i H.@?= True   --
    , H.testCase "Disjunction 3" $     --
        evaluateIn dis3 i H.@?= True   --
    , H.testCase "Disjunction 4" $     --
        evaluateIn dis4 i H.@?= True   --
    , H.testCase "Disjunction 5" $     --
        evaluateIn dis5 i H.@?= True   --
    , H.testCase "Disjunction 6" $     --
        evaluateIn dis6 i H.@?= True   --
    , H.testCase "Disjunction 7" $     --
        evaluateIn dis7 i H.@?= True   --
    , H.testCase "Disjunction 8" $     --
        evaluateIn dis8 i H.@?= True   --
    , H.testCase "Disjunction 9" $     --
        evaluateIn dis9 i H.@?= False  --
    -------------------------------------
                                       --
    ---- Implications -------------------
    , H.testCase "Implication 1" $     --
        evaluateIn imp1 i H.@?= True   --
    , H.testCase "Implication 2" $     --
        evaluateIn imp2 i H.@?= True   --
    , H.testCase "Implication 3" $     --
        evaluateIn imp3 i H.@?= False  --
    , H.testCase "Implication 4" $     --
        evaluateIn imp4 i H.@?= True   --
    , H.testCase "Implication 5" $     --
        evaluateIn imp5 i H.@?= True   --
    , H.testCase "Implication 6" $     --
        evaluateIn imp6 i H.@?= False  --
    , H.testCase "Implication 7" $     --
        evaluateIn imp7 i H.@?= True   --
    , H.testCase "Implication 8" $     --
        evaluateIn imp8 i H.@?= True   --
    , H.testCase "Implication 9" $     --
        evaluateIn imp9 i H.@?= True   --
    -------------------------------------
                                       --
    ---- Universals ---------------------
    , H.testCase "Universal 1" $       --
        evaluateIn uni1 i H.@?= True   --
    , H.testCase "Universal  2" $      --
        evaluateIn uni2 i H.@?= False   --
    , H.testCase "Universal  3" $      --
        evaluateIn uni3 i H.@?= False  --
    , H.testCase "Universal  4" $      --
        evaluateIn uni4 i H.@?= False  --
    , H.testCase "Universal  5" $      --
        evaluateIn uni5 i H.@?= True   --
    , H.testCase "Universal  6" $      --
        evaluateIn uni6 i H.@?= True   --
    -------------------------------------
                                       --
    ---- Existentials -------------------
    , H.testCase "Existential 1" $     --
        evaluateIn exi1 i H.@?= True   --
    , H.testCase "Existential  2" $    --
        evaluateIn exi2 i H.@?= False  --
    , H.testCase "Existential  3" $    --
        evaluateIn exi3 i H.@?= True   --
    , H.testCase "Existential  4" $    --
        evaluateIn exi4 i H.@?= True   --
    , H.testCase "Existential  5" $    --
        evaluateIn exi5 i H.@?= False  --
    , H.testCase "Universal  6" $      --
        evaluateIn exi6 i H.@?= True   --
    ---------------------------------------
    ]
    where 
        -- Predicates
        pred1 :: QuantificationalFormula
        pred1 = Predicate "Dead" ["Laura"]
        pred2 :: QuantificationalFormula
        pred2 = Predicate "Killed" ["BOB", "Laura"]
        pred3 :: QuantificationalFormula
        pred3 = Predicate "Loves" ["Cooper", "Windom"]
        -- Equality
        equ1 :: QuantificationalFormula
        equ1 = Equality "Cooper" "Cooper"
        equ2 :: QuantificationalFormula
        equ2 = Equality "Cooper" "Evil Cooper"
        equ3 :: QuantificationalFormula
        equ3 = Equality "Cooper" "Audrey"
        equ4 :: QuantificationalFormula
        equ4 = Equality "Audrey" "Audrey"
        -- Negations
        neg1 :: QuantificationalFormula
        neg1 = Negation pred1 
        neg2 :: QuantificationalFormula
        neg2 = Negation pred2 
        neg3 :: QuantificationalFormula
        neg3 = Negation pred3 
        -- Conjunctions
        con1 :: QuantificationalFormula
        con1 = Conjunction pred1 pred1
        con2 :: QuantificationalFormula
        con2 = Conjunction pred1 pred2 
        con3 :: QuantificationalFormula
        con3 = Conjunction pred1 pred3
        con4 :: QuantificationalFormula
        con4 = Conjunction pred2 pred1
        con5 :: QuantificationalFormula
        con5 = Conjunction pred2 pred2
        con6 :: QuantificationalFormula
        con6 = Conjunction pred2 pred3
        con7 :: QuantificationalFormula
        con7 = Conjunction pred3 pred1
        con8 :: QuantificationalFormula
        con8 = Conjunction pred3 pred2
        con9 :: QuantificationalFormula
        con9 = Conjunction pred3 pred3
        -- Disjunction
        dis1 :: QuantificationalFormula
        dis1 = Disjunction pred1 pred1
        dis2 :: QuantificationalFormula
        dis2 = Disjunction pred1 pred2 
        dis3 :: QuantificationalFormula
        dis3 = Disjunction pred1 pred3
        dis4 :: QuantificationalFormula
        dis4 = Disjunction pred2 pred1
        dis5 :: QuantificationalFormula
        dis5 = Disjunction pred2 pred2
        dis6 :: QuantificationalFormula
        dis6 = Disjunction pred2 pred3
        dis7 :: QuantificationalFormula
        dis7 = Disjunction pred3 pred1
        dis8 :: QuantificationalFormula
        dis8 = Disjunction pred3 pred2
        dis9 :: QuantificationalFormula
        dis9 = Disjunction pred3 pred3
        -- Implications
        imp1 :: QuantificationalFormula
        imp1 = Implication pred1 pred1
        imp2 :: QuantificationalFormula
        imp2 = Implication pred1 pred2 
        imp3 :: QuantificationalFormula
        imp3 = Implication pred1 pred3
        imp4 :: QuantificationalFormula
        imp4 = Implication pred2 pred1
        imp5 :: QuantificationalFormula
        imp5 = Implication pred2 pred2
        imp6 :: QuantificationalFormula
        imp6 = Implication pred2 pred3
        imp7 :: QuantificationalFormula
        imp7 = Implication pred3 pred1
        imp8 :: QuantificationalFormula
        imp8 = Implication pred3 pred2
        imp9 :: QuantificationalFormula
        imp9 = Implication pred3 pred3
        -- Universals
        uni1 :: QuantificationalFormula
        uni1 = Universal $ \x -> 
                   Disjunction 
                       (Predicate "Alive" [x]) 
                       (Predicate "Dead" [x])
        uni2 :: QuantificationalFormula
        uni2 = Universal $ \x -> 
                   Predicate "Happy" [x]
        uni3 :: QuantificationalFormula
        uni3 = Universal $ \x -> 
                   Conjunction 
                       (Predicate "InTwinPeaks" [x]) 
                       (Predicate "Loves" [x,x])
        uni4 :: QuantificationalFormula
        uni4 = Universal $ \x -> 
                   Conjunction 
                       (Negation 
                           (Predicate "InTwinPeaks" [x])) 
                        (Predicate "Loves" [x,"Gordon"])
        uni5 :: QuantificationalFormula
        uni5 = Universal $ \x -> 
                   Disjunction 
                       (Predicate "Killed" ["BOB", x]) 
                       (Disjunction 
                           (Predicate "InTwinPeaks" [x]) 
                           (Predicate "InTheBlackLodge" [x]))
        uni6 :: QuantificationalFormula
        uni6 = Universal $ \x -> 
                   Negation 
                       (Predicate "Happy" [x])
        -- Existential
        exi1 :: QuantificationalFormula
        exi1 = Existential $ \x -> 
                   Predicate "Killed" ["BOB", x]
        exi2 :: QuantificationalFormula
        exi2 = Existential $ \x -> 
                   Predicate "Killed" ["Cooper", x]
        exi3 :: QuantificationalFormula
        exi3 = Existential $ \x -> 
                   Conjunction 
                       (Predicate "EatsGarmonbozia" [x]) 
                       (Predicate "InTwinPeaks" [x])
        exi4 :: QuantificationalFormula
        exi4 = Existential $ \x ->
                   Disjunction 
                       (Predicate "StudentAtTwinPeaksHigh" [x])
                       (Predicate "InTheBlackLodge" [x])
        exi5 :: QuantificationalFormula
        exi5 = Existential $ \x ->
                   Predicate "LovesBlackCoffee" [x]
        exi6 :: QuantificationalFormula
        exi6 = Existential $ \x -> 
                   Existential $ \y -> 
                       Conjunction 
                           (Predicate "Loves" [x, y]) 
                           (Predicate "Loves" [y, x])