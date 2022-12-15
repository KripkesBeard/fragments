module Semantics.Logic.Propositional.PrettyPrinting
    ( pPrint
    ) where

import Semantics.Logic.Propositional.Syntax 
    ( PropositionalFormula (..) 
    )


import Text.Parsec 
    (
    ) 

-- TODO: everything 

pPrint :: PropositionalFormula -> String
pPrint (Atomic p)             = show p
pPrint (Negation (Atomic p))  = "~" <> show p
pPrint (Negation p)           = "~(" <> pPrint p <> ")"
pPrint (Conjunction p q)      =  "(" <> pPrint p <> " /\\ " <> pPrint q <> ")"
pPrint (Disjunction p q)      =  "(" <> pPrint p <> " \\/ " <> pPrint q <> ")"
pPrint (Implication p q)      =  "(" <> pPrint p <> " ~> "  <> pPrint q <> ")"