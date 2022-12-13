module Semantics.Logic.PropositionalSyntax 
    ( PropositionalVariable 
    , PropositionalFormula (..)
    ) where

type PropositionalVariable = String

data PropositionalFormula
    = Atomic      !PropositionalVariable
    | Negation    !PropositionalFormula
    | Conjunction !PropositionalFormula !PropositionalFormula
    | Disjunction !PropositionalFormula !PropositionalFormula
    | Implication !PropositionalFormula !PropositionalFormula
    deriving (Eq)

instance Show PropositionalFormula where
    show (Atomic p)        = p
    show (Negation p)      = "~(" <> show p <> ")"
    show (Conjunction p q) =  "(" <> show p <> " ^ "  <> show q <> ")"
    show (Disjunction p q) =  "(" <> show p <> " v "  <> show q <> ")"
    show (Implication p q) =  "(" <> show p <> " -> " <> show q <> ")"

-- TODO 
-- Implement a parser to use as instance for Read
-- then once I do, export another function that's just a synonym for read which is the syntax equivalent of exporting makeInterpretation