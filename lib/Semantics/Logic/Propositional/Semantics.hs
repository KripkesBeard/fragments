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

import Data.Set (Set)
import qualified Data.Set as Set

import Semantics.Logic.Propositional.Syntax 
    ( PropositionalVariable
    , PropositionalFormula (..) 
    )

-- | An interpretation for a boolean logic is the set of the propositional variables which are true.
type Interpretation = Set PropositionalVariable

-- | Create an interpretation
createInterpretation :: [PropositionalVariable] -> Interpretation
createInterpretation = Set.fromList


-- | Evaluates a propositional formula in an interpretation.
--
-- Let i be an interpretation such that 
--
-- @
-- i "a" -> True
-- i "b" -> False
-- i "c" -> True
-- @
--
-- Then
-- 
-- >>> evaluateIn (Negation (Implication (Disjunction (Atomic "a") (Conjunction (Negation (Atomic "b")) (Atomic "c"))) (Atomic "b"))) i
-- True
--
evaluateIn :: PropositionalFormula -> Interpretation -> Bool
evaluateIn (Atomic p)        i = p `Set.member` i
evaluateIn (Negation p)      i = not (evaluateIn p i)
evaluateIn (Conjunction p q) i = evaluateIn p i && evaluateIn q i
evaluateIn (Disjunction p q) i = evaluateIn p i || evaluateIn q i
evaluateIn (Implication p q) i = not (evaluateIn p i) || evaluateIn q i