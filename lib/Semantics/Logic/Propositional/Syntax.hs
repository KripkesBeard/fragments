{-|
This is the "internal" module for propositional logic syntax. See the main Semantics.Logic.Propositional module 
for an explanation of what is going on.
-}

module Semantics.Logic.Propositional.Syntax
    ( -- * Abstract Syntax
      PropositionalVariable 
    , PropositionalFormula (..)
    ) where

-- | A propositional variable is a name
type PropositionalVariable = String

-- | A propositional formula is a well formed abstract syntax tree encoding its logical form.
data PropositionalFormula
    = Atomic      PropositionalVariable
    | Negation    PropositionalFormula
    | Conjunction PropositionalFormula PropositionalFormula
    | Disjunction PropositionalFormula PropositionalFormula
    | Implication PropositionalFormula PropositionalFormula
    deriving (Eq, Show, Read)

toString :: PropositionalFormula -> String
toString (Atomic p)             = show p
toString (Negation (Atomic p))  = "~" <> show p
toString (Negation p)           = "~(" <> toString p <> ")"
toString (Conjunction p q)      =  "(" <> toString p <> " /\\ " <> toString q <> ")"
toString (Disjunction p q)      =  "(" <> toString p <> " \\/ " <> toString q <> ")"
toString (Implication p q)      =  "(" <> toString p <> " => "  <> toString q <> ")"

prettyPrint :: PropositionalFormula -> IO ()
prettyPrint = putStrLn . toString