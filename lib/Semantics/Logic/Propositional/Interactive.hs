module Semantics.Logic.Propositional.Interactive
    ( pPrint
    ) where

import Semantics.Logic.Propositional.Syntax 
    ( PropositionalFormula (..) 
    )


import Text.Parsec 
    (
    ) 

-- Idea is to have some functions that takes a list of interpretations and a list of formulas and evaluates each of the formulas at each of the interpretations 
-- Two ways, one just takes those two lists, the other reads from a csv?

-- evaluateAt needs the last argument to be a PropositionalFormula e.i. it needs to be like (Atomic "A") etc, so maybe export another function here
-- which takes an interpretation but also a string as an argument in regular notation and then feeds i and (parse string) into evaluateAt ? In that case I should actually just
-- rework evaluateAt, maybe? I don't know because I probably do still need an internal function that evaluates a raw PropositionalFormula expression, so maybe still
-- export something different. No idea

pPrint :: PropositionalFormula -> String
pPrint (Atomic p)             = p
pPrint (Negation (Atomic p))  = "~" <> p
pPrint (Negation p)           = "~(" <> pPrint p <> ")"
pPrint (Conjunction p q)      =  "(" <> pPrint p <> " /\\ " <> pPrint q <> ")"
pPrint (Disjunction p q)      =  "(" <> pPrint p <> " \\/ " <> pPrint q <> ")"
pPrint (Implication p q)      =  "(" <> pPrint p <> " ~> "  <> pPrint q <> ")"