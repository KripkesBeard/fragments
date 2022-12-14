module Semantics.Logic.PropositionalModal.Syntax
  ( PropositionalVariable
  , PropositionalModalFormula (..)
  ) where

type PropositionalVariable = String

data PropositionalModalFormula
    = Atomic      !PropositionalVariable
    | Negation    !PropositionalModalFormula
    | Conjunction !PropositionalModalFormula !PropositionalModalFormula
    | Disjunction !PropositionalModalFormula !PropositionalModalFormula
    | Implication !PropositionalModalFormula !PropositionalModalFormula
    | Necessity   !PropositionalModalFormula
    | Possibility !PropositionalModalFormula
    deriving (Eq, Show, Read)