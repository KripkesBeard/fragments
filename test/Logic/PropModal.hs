module Logic.PropModal (tests) where

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
import Semantics.Logic.PropositionalModal.Syntax
    ( PropositionalVariable
    , PropositionalModalFormula (..)
    )
import Semantics.Logic.PropositionalModal.Semantics 
    ( World
    , createWorld
    , Worlds
    , createWorlds
    , AccessibilityRelation
    , createAccessibilityRelation
    , KripkeFrame
    , createKripkeFrame
    , TruePropositions
    , createTruePropositions
    , Interpretation
    , createInterpretation
    , KripkeModel
    , createKripkeModel
    , evaluateAtIn
    )



-- | Main test tree containing unit tests and property based tests
-- for propositional modal logic
tests :: TestTree
tests = testGroup "All tests" [unitTests]

-- Model creation
totalWorlds :: Worlds
totalWorlds = createWorlds [1,2,3] 

worlds1 :: Worlds
worlds1 = createWorlds [2]
worlds2 :: Worlds
worlds2 = createWorlds [2,3]
worlds3 :: Worlds
worlds3 = createWorlds []

accessibilityRelation :: AccessibilityRelation
accessibilityRelation = createAccessibilityRelation [(1, worlds1)
                                                    ,(2, worlds2)
                                                    ,(3, worlds3)
                                                    ]

kf :: KripkeFrame
kf = createKripkeFrame totalWorlds accessibilityRelation

prop1 :: TruePropositions
prop1 = createTruePropositions ["a"]
prop2 :: TruePropositions
prop2 = createTruePropositions ["a","b"]
prop3 :: TruePropositions
prop3 = createTruePropositions ["b","c"]

i :: Interpretation
i = createInterpretation [(1, prop1), (2, prop2), (3, prop3)]

m :: KripkeModel
m = createKripkeModel kf i

-- | Unit tests for propositional modal logic
unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [  
    ---- Atomics --------------------------------
    ---- world 1 --------------------------------
      H.testCase "Atomic 1 at world 1" $       --
        evaluateAtIn atom1 1 m H.@?= True      --
    , H.testCase "Atomic 2 at world 1" $       --
        evaluateAtIn atom2 1 m H.@?= False     --
    , H.testCase "Atomic 3 at world 1" $       --
        evaluateAtIn atom3 1 m H.@?= False     --
    ---- world 2 --------------------------------
    , H.testCase "Atomic 1 at world 2" $       --
        evaluateAtIn atom1 2 m H.@?= True      --
    , H.testCase "Atomic 2 at world 2" $       --
        evaluateAtIn atom2 2 m H.@?= True      --
    , H.testCase "Atomic 3 at world 2" $       --
        evaluateAtIn atom3 2 m H.@?= False     --
    ---- world 3 --------------------------------
    , H.testCase "Atomic 1 at world 3" $       --
        evaluateAtIn atom1 3 m H.@?= False     --
    , H.testCase "Atomic 2 at world 3" $       --
        evaluateAtIn atom2 3 m H.@?= True      --
    , H.testCase "Atomic 3 at world 3" $       --
        evaluateAtIn atom3 3 m H.@?= True      --
    ---------------------------------------------
                                               --
    ---- Negations ------------------------------
    ---- world 1 --------------------------------
    , H.testCase "Negation 1 at world 1" $     --
        evaluateAtIn neg1 1 m H.@?= False      --
    , H.testCase "Negation 2 at world 1" $     --
        evaluateAtIn neg2 1 m H.@?= True       --
    , H.testCase "Negation 3 at world 1" $     --
        evaluateAtIn neg3 1 m H.@?= True       --
    ---- world 2 --------------------------------
    , H.testCase "Negation 1 at world 2" $     --
        evaluateAtIn neg1 2 m H.@?= False      --
    , H.testCase "Negation 2 at world 2" $     --
        evaluateAtIn neg2 2 m H.@?= False      --
    , H.testCase "Negation 3 at world 2" $     --
        evaluateAtIn neg3 2 m H.@?= True       --
    ---- world 3 --------------------------------
    , H.testCase "Negation 1 at world 3" $     --
        evaluateAtIn neg1 3 m H.@?= True       --
    , H.testCase "Negation 2 at world 3" $     --
        evaluateAtIn neg2 3 m H.@?= False      --
    , H.testCase "Negation 3 at world 3" $     --
        evaluateAtIn neg3 3 m H.@?= False      --
    ---------------------------------------------
                                               --
    ---- Conjunctions ---------------------------
    ---- world 1 --------------------------------
    , H.testCase "Conjunction 1 at world 1" $  --
        evaluateAtIn con1 1 m H.@?= True       --
    , H.testCase "Conjunction 2 at world 1" $  --
        evaluateAtIn con2 1 m H.@?= False      --
    , H.testCase "Conjunction 3 at world 1" $  --
        evaluateAtIn con3 1 m H.@?= False      --
    , H.testCase "Conjunction 4 at world 1" $  --
        evaluateAtIn con4 1 m H.@?= False      --
    , H.testCase "Conjunction 5 at world 1" $  --
        evaluateAtIn con5 1 m H.@?= False      --
    , H.testCase "Conjunction 6 at world 1" $  --
        evaluateAtIn con6 1 m H.@?= False      --
    , H.testCase "Conjunction 7 at world 1" $  --
        evaluateAtIn con7 1 m H.@?= False      --
    , H.testCase "Conjunction 8 at world 1" $  --
        evaluateAtIn con8 1 m H.@?= False      --
    , H.testCase "Conjunction 9 at world 1" $  --
        evaluateAtIn con9 1 m H.@?= False      --
    ---- world 2 --------------------------------
    , H.testCase "Conjunction 1 at world 2" $  --
        evaluateAtIn con1 2 m H.@?= True       --
    , H.testCase "Conjunction 2 at world 2" $  --
        evaluateAtIn con2 2 m H.@?= True       --
    , H.testCase "Conjunction 3 at world 2" $  --
        evaluateAtIn con3 2 m H.@?= False      --
    , H.testCase "Conjunction 4 at world 2" $  --
        evaluateAtIn con4 2 m H.@?= True       --
    , H.testCase "Conjunction 5 at world 2" $  --
        evaluateAtIn con5 2 m H.@?= True       --
    , H.testCase "Conjunction 6 at world 2" $  --
        evaluateAtIn con6 2 m H.@?= False      --
    , H.testCase "Conjunction 7 at world 2" $  --
        evaluateAtIn con7 2 m H.@?= False      --
    , H.testCase "Conjunction 8 at world 2" $  --
        evaluateAtIn con8 2 m H.@?= False      --
    , H.testCase "Conjunction 9 at world 2" $  --
        evaluateAtIn con9 2 m H.@?= False      --
    ---- world 3 --------------------------------
    , H.testCase "Conjunction 1 at world 3" $  --
        evaluateAtIn con1 3 m H.@?= False      --
    , H.testCase "Conjunction 2 at world 3" $  --
        evaluateAtIn con2 3 m H.@?= False      --
    , H.testCase "Conjunction 3 at world 3" $  --
        evaluateAtIn con3 3 m H.@?= False      --
    , H.testCase "Conjunction 4 at world 3" $  --
        evaluateAtIn con4 3 m H.@?= False      --
    , H.testCase "Conjunction 5 at world 3" $  --
        evaluateAtIn con5 3 m H.@?= True       --
    , H.testCase "Conjunction 6 at world 3" $  --
        evaluateAtIn con6 3 m H.@?= True       --
    , H.testCase "Conjunction 7 at world 3" $  --
        evaluateAtIn con7 3 m H.@?= False      --
    , H.testCase "Conjunction 8 at world 3" $  --
        evaluateAtIn con8 3 m H.@?= True       --
    , H.testCase "Conjunction 9 at world 3" $  --
        evaluateAtIn con9 3 m H.@?= True       --
    ---------------------------------------------
                                               --
    ---- Disjunctions ---------------------------
    ---- world 1 --------------------------------
    , H.testCase "Disjunction 1 at world 1" $  --
        evaluateAtIn dis1 1 m H.@?= True       --
    , H.testCase "Disjunction 2 at world 1" $  --
        evaluateAtIn dis2 1 m H.@?= True       --
    , H.testCase "Disjunction 3 at world 1" $  --
        evaluateAtIn dis3 1 m H.@?= True       --
    , H.testCase "Disjunction 4 at world 1" $  --
        evaluateAtIn dis4 1 m H.@?= True       --
    , H.testCase "Disjunction 5 at world 1" $  --
        evaluateAtIn dis5 1 m H.@?= False      --
    , H.testCase "Disjunction 6 at world 1" $  --
        evaluateAtIn dis6 1 m H.@?= False      --
    , H.testCase "Disjunction 7 at world 1" $  --
        evaluateAtIn dis7 1 m H.@?= True       --
    , H.testCase "Disjunction 8 at world 1" $  --
        evaluateAtIn dis8 1 m H.@?= False      --
    , H.testCase "Disjunction 9 at world 1" $  --
        evaluateAtIn dis9 1 m H.@?= False      --
    ---- world 2 --------------------------------
    , H.testCase "Disjunction 1 at world 2" $  --
        evaluateAtIn dis1 2 m H.@?= True       --
    , H.testCase "Disjunction 2 at world 2" $  --
        evaluateAtIn dis2 2 m H.@?= True       --
    , H.testCase "Disjunction 3 at world 2" $  --
        evaluateAtIn dis3 2 m H.@?= True       --
    , H.testCase "Disjunction 4 at world 2" $  --
        evaluateAtIn dis4 2 m H.@?= True       --
    , H.testCase "Disjunction 5 at world 2" $  --
        evaluateAtIn dis5 2 m H.@?= True       --
    , H.testCase "Disjunction 6 at world 2" $  --
        evaluateAtIn dis6 2 m H.@?= True       --
    , H.testCase "Disjunction 7 at world 2" $  --
        evaluateAtIn dis7 2 m H.@?= True       --
    , H.testCase "Disjunction 8 at world 2" $  --
        evaluateAtIn dis8 2 m H.@?= True       --
    , H.testCase "Disjunction 9 at world 2" $  --
        evaluateAtIn dis9 2 m H.@?= False      --
    ---- world 3 --------------------------------
    , H.testCase "Disjunction 1 at world 3" $  --
        evaluateAtIn dis1 3 m H.@?= False      --
    , H.testCase "Disjunction 2 at world 3" $  --
        evaluateAtIn dis2 3 m H.@?= True       --
    , H.testCase "Disjunction 3 at world 3" $  --
        evaluateAtIn dis3 3 m H.@?= True       --
    , H.testCase "Disjunction 4 at world 3" $  --
        evaluateAtIn dis4 3 m H.@?= True       --
    , H.testCase "Disjunction 5 at world 3" $  --
        evaluateAtIn dis5 3 m H.@?= True       --
    , H.testCase "Disjunction 6 at world 3" $  --
        evaluateAtIn dis6 3 m H.@?= True       --
    , H.testCase "Disjunction 7 at world 3" $  --
        evaluateAtIn dis7 3 m H.@?= True       --
    , H.testCase "Disjunction 8 at world 3" $  --
        evaluateAtIn dis8 3 m H.@?= True       --
    , H.testCase "Disjunction 9 at world 3" $  --
        evaluateAtIn dis9 3 m H.@?= True       --
    ---------------------------------------------
                                               --
    ---- Implications ---------------------------
    ---- world 1 --------------------------------
    , H.testCase "Implication 1 at world 1" $  --
        evaluateAtIn imp1 1 m H.@?= True       --
    , H.testCase "Implication 2 at world 1" $  --
        evaluateAtIn imp2 1 m H.@?= False      --
    , H.testCase "Implication 3 at world 1" $  --
        evaluateAtIn imp3 1 m H.@?= False      --
    , H.testCase "Implication 4 at world 1" $  --
        evaluateAtIn imp4 1 m H.@?= True       --
    , H.testCase "Implication 5 at world 1" $  --
        evaluateAtIn imp5 1 m H.@?= True       --
    , H.testCase "Implication 6 at world 1" $  --
        evaluateAtIn imp6 1 m H.@?= True       --
    , H.testCase "Implication 7 at world 1" $  --
        evaluateAtIn imp7 1 m H.@?= True       --
    , H.testCase "Implication 8 at world 1" $  --
        evaluateAtIn imp8 1 m H.@?= True       --
    , H.testCase "Implication 9 at world 1" $  --
        evaluateAtIn imp9 1 m H.@?= True       --
    ---- world 2 --------------------------------
    , H.testCase "Implication 1 at world 2" $  --
        evaluateAtIn imp1 2 m H.@?= True       --
    , H.testCase "Implication 2 at world 2" $  --
        evaluateAtIn imp2 2 m H.@?= True       --
    , H.testCase "Implication 3 at world 2" $  --
        evaluateAtIn imp3 2 m H.@?= False      --
    , H.testCase "Implication 4 at world 2" $  --
        evaluateAtIn imp4 2 m H.@?= True       --
    , H.testCase "Implication 5 at world 2" $  --
        evaluateAtIn imp5 2 m H.@?= True       --
    , H.testCase "Implication 6 at world 2" $  --
        evaluateAtIn imp6 2 m H.@?= False      --
    , H.testCase "Implication 7 at world 2" $  --
        evaluateAtIn imp7 2 m H.@?= True       --
    , H.testCase "Implication 8 at world 2" $  --
        evaluateAtIn imp8 2 m H.@?= True       --
    , H.testCase "Implication 9 at world 2" $  --
        evaluateAtIn imp9 2 m H.@?= True       --
    ---- world 3 --------------------------------
    , H.testCase "Implication 1 at world 3" $  --
        evaluateAtIn imp1 3 m H.@?= True       --
    , H.testCase "Implication 2 at world 3" $  --
        evaluateAtIn imp2 3 m H.@?= True       --
    , H.testCase "Implication 3 at world 3" $  --
        evaluateAtIn imp3 3 m H.@?= True       --
    , H.testCase "Implication 4 at world 3" $  --
        evaluateAtIn imp4 3 m H.@?= False      --
    , H.testCase "Implication 5 at world 3" $  --
        evaluateAtIn imp5 3 m H.@?= True       --
    , H.testCase "Implication 6 at world 3" $  --
        evaluateAtIn imp6 3 m H.@?= True       --
    , H.testCase "Implication 7 at world 3" $  --
        evaluateAtIn imp7 3 m H.@?= False      --
    , H.testCase "Implication 8 at world 3" $  --
        evaluateAtIn imp8 3 m H.@?= True       --
    , H.testCase "Implication 9 at world 3" $  --
        evaluateAtIn imp9 3 m H.@?= True       --
    ---------------------------------------------
                                               --
    ---- Necessity ------------------------------
    ---- world 1 --------------------------------
    , H.testCase "Necessity 1 at world 1" $    --
        evaluateAtIn nec1 1 m H.@?= True       --
    , H.testCase "Necessity 2 at world 1" $    --
        evaluateAtIn nec2 1 m H.@?= True       --
    , H.testCase "Necessity 3 at world 1" $    --
        evaluateAtIn nec3 1 m H.@?= False      --
    ---- world 2 --------------------------------
    , H.testCase "Necessity 1 at world 2" $    --
        evaluateAtIn nec1 2 m H.@?= False      --
    , H.testCase "Necessity 2 at world 2" $    --
        evaluateAtIn nec2 2 m H.@?= True       --
    , H.testCase "Necessity 3 at world 2" $    --
        evaluateAtIn nec3 2 m H.@?= False      --
    ---- world 3 --------------------------------
    , H.testCase "Necessity 1 at world 3" $    --
        evaluateAtIn nec1 3 m H.@?= True       --
    , H.testCase "Necessity 2 at world 3" $    --
        evaluateAtIn nec2 3 m H.@?= True       --
    , H.testCase "Necessity 3 at world 3" $    --
        evaluateAtIn nec3 3 m H.@?= True       --
    ---------------------------------------------
                                               --
    ---- Possibility ----------------------------
    ---- world 1 --------------------------------
    , H.testCase "Possibility 1 at world 1" $  --
        evaluateAtIn pos1 1 m H.@?= True       --
    , H.testCase "Possibility 2 at world 1" $  --
        evaluateAtIn pos2 1 m H.@?= True       --
    , H.testCase "Possibility 3 at world 1" $  --
        evaluateAtIn pos3 1 m H.@?= False      --
    ---- world 2 --------------------------------
    , H.testCase "Possibility 1 at world 2" $  --
        evaluateAtIn pos1 2 m H.@?= True       --
    , H.testCase "Possibility 2 at world 2" $  --
        evaluateAtIn pos2 2 m H.@?= True       --
    , H.testCase "Possibility 3 at world 2" $  --
        evaluateAtIn pos3 2 m H.@?= True       --
    ---- world 3 --------------------------------
    , H.testCase "Possibility 1 at world 3" $  --
        evaluateAtIn pos1 3 m H.@?= False      --
    , H.testCase "Possibility 2 at world 3" $  --
        evaluateAtIn pos2 3 m H.@?= False      --
    , H.testCase "Possibility 3 at world 3" $  --
        evaluateAtIn pos3 3 m H.@?= False      --
    ---------------------------------------------
    ]
    where
        -- Atomics
        atom1 :: PropositionalModalFormula
        atom1 = Atomic "a"
        atom2 :: PropositionalModalFormula
        atom2 = Atomic "b"
        atom3 :: PropositionalModalFormula
        atom3 = Atomic "c"
        -- Negations
        neg1 :: PropositionalModalFormula
        neg1 = Negation atom1
        neg2 :: PropositionalModalFormula
        neg2 = Negation atom2
        neg3 :: PropositionalModalFormula
        neg3 = Negation atom3
        -- Conjunctions
        con1 :: PropositionalModalFormula
        con1 = Conjunction atom1 atom1
        con2 :: PropositionalModalFormula
        con2 = Conjunction atom1 atom2
        con3 :: PropositionalModalFormula
        con3 = Conjunction atom1 atom3
        con4 :: PropositionalModalFormula
        con4 = Conjunction atom2 atom1
        con5 :: PropositionalModalFormula
        con5 = Conjunction atom2 atom2
        con6 :: PropositionalModalFormula
        con6 = Conjunction atom2 atom3
        con7 :: PropositionalModalFormula
        con7 = Conjunction atom3 atom1
        con8 :: PropositionalModalFormula
        con8 = Conjunction atom3 atom2
        con9 :: PropositionalModalFormula
        con9 = Conjunction atom3 atom3
        -- Disjunctions
        dis1 :: PropositionalModalFormula
        dis1 = Disjunction atom1 atom1
        dis2 :: PropositionalModalFormula
        dis2 = Disjunction atom1 atom2
        dis3 :: PropositionalModalFormula
        dis3 = Disjunction atom1 atom3
        dis4 :: PropositionalModalFormula
        dis4 = Disjunction atom2 atom1
        dis5 :: PropositionalModalFormula
        dis5 = Disjunction atom2 atom2
        dis6 :: PropositionalModalFormula
        dis6 = Disjunction atom2 atom3
        dis7 :: PropositionalModalFormula
        dis7 = Disjunction atom3 atom1
        dis8 :: PropositionalModalFormula
        dis8 = Disjunction atom3 atom2
        dis9 :: PropositionalModalFormula
        dis9 = Disjunction atom3 atom3
        -- Implications
        imp1 :: PropositionalModalFormula
        imp1 = Implication atom1 atom1
        imp2 :: PropositionalModalFormula
        imp2 = Implication atom1 atom2
        imp3 :: PropositionalModalFormula
        imp3 = Implication atom1 atom3
        imp4 :: PropositionalModalFormula
        imp4 = Implication atom2 atom1
        imp5 :: PropositionalModalFormula
        imp5 = Implication atom2 atom2
        imp6 :: PropositionalModalFormula
        imp6 = Implication atom2 atom3
        imp7 :: PropositionalModalFormula
        imp7 = Implication atom3 atom1
        imp8 :: PropositionalModalFormula
        imp8 = Implication atom3 atom2
        imp9 :: PropositionalModalFormula
        imp9 = Implication atom3 atom3
        -- Necessities
        nec1 :: PropositionalModalFormula
        nec1 = Necessity atom1
        nec2 :: PropositionalModalFormula
        nec2 = Necessity atom2
        nec3 :: PropositionalModalFormula
        nec3 = Necessity atom3
        -- Possibilities
        pos1 :: PropositionalModalFormula
        pos1 = Possibility atom1
        pos2 :: PropositionalModalFormula
        pos2 = Possibility atom2
        pos3 :: PropositionalModalFormula
        pos3 = Possibility atom3