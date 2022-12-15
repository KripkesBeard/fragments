{-|
This is the "internal" module for propositional logic semantics. See the main Semantics.Logic.Propositional module 
for an explanation of what is going on.
-}

module Semantics.Logic.Propositional.Semantics
    ( -- * Model Structure
      Interpretation
    , createInterpretation
    , -- * Evaluation
      evaluateIn
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Semantics.Logic.Propositional.Syntax 
    ( PropositionalVariable
    , PropositionalFormula (..) 
    )

-- | An interpretation for a boolean logic is a mapping from propositional variables to boolean values.
type Interpretation = Map PropositionalVariable Bool

-- | Create an interpretation
createInterpretation :: [(PropositionalVariable, Bool)] -> Interpretation
createInterpretation = Map.fromList


-- | Evaluates a propositional formula in an interpretation.
--
-- Let i be an interpretation such that 
--
-- @
-- i 1 -> True
-- i 2 -> False
-- i 3 -> True
-- @
--
-- Then
-- 
-- >>> evaluateIn (Negation (Implication (Disjunction (Atomic 1) (Conjunction (Negation (Atomic 2)) (Atomic 3))) (Atomic 2))) i
-- True
--
evaluateIn :: PropositionalFormula -> Interpretation -> Bool
evaluateIn (Atomic p)        i = i Map.! p
evaluateIn (Negation p)      i = not (evaluateIn p i)
evaluateIn (Conjunction p q) i = evaluateIn p i && evaluateIn q i
evaluateIn (Disjunction p q) i = evaluateIn p i || evaluateIn q i
evaluateIn (Implication p q) i = not (evaluateIn p i) || evaluateIn q i