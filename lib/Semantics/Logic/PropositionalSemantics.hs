module Semantics.Logic.PropositionalSemantics 
    ( Interpretation
    , makeInterpretation
    , evaluateAt
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Semantics.Logic.PropositionalSyntax 
    ( PropositionalVariable
    , PropositionalFormula (..) 
    )

type Interpretation = Map PropositionalVariable Bool

makeInterpretation ::[(PropositionalVariable, Bool)] -> Interpretation
makeInterpretation = Map.fromList 

evaluateAt :: Interpretation -> PropositionalFormula -> Bool
evaluateAt interpretation (Atomic p)        = interpretation Map.! p
evaluateAt interpretation (Negation p)      = not (evaluateAt interpretation p)
evaluateAt interpretation (Conjunction p q) = evaluateAt interpretation p && evaluateAt interpretation q
evaluateAt interpretation (Disjunction p q) = evaluateAt interpretation p || evaluateAt interpretation q
evaluateAt interpretation (Implication p q) = not (evaluateAt interpretation p) || evaluateAt interpretation q