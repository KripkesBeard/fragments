module Semantics.Logic.Propositional.Semantics
    ( Interpretation
    , makeInterpretation
    , evaluateAt
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Semantics.Logic.Propositional.Syntax 
    ( PropositionalVariable
    , PropositionalFormula (..) 
    )

type Interpretation = Map PropositionalVariable Bool

makeInterpretation ::[(PropositionalVariable, Bool)] -> Interpretation
makeInterpretation = Map.fromList 

evaluateAt :: Interpretation -> PropositionalFormula -> Bool
evaluateAt i (Atomic p)        = i Map.! p
evaluateAt i (Negation p)      = not (evaluateAt i p)
evaluateAt i (Conjunction p q) = evaluateAt i p && evaluateAt i q
evaluateAt i (Disjunction p q) = evaluateAt i p || evaluateAt i q
evaluateAt i (Implication p q) = not (evaluateAt i p) || evaluateAt i q