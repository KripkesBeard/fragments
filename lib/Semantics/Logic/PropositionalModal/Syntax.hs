{-|
This is the "internal" module for propositional modal logic syntax. See the main Semantics.Logic.PropositionalModal module 
for an explanation of what is going on.
-}


module Semantics.Logic.PropositionalModal.Syntax
  ( -- * Abstract Syntax
    PropositionalVariable
  , PropositionalModalFormula (..)
  ) where

-- | A PropositionalVariable is a point.
type PropositionalVariable = Int

-- | A propositional modal formula is a well formed abstract syntax tree encoding its logical form.
data PropositionalModalFormula
    = Atomic      !PropositionalVariable 
    | Negation    !PropositionalModalFormula
    | Conjunction !PropositionalModalFormula !PropositionalModalFormula
    | Disjunction !PropositionalModalFormula !PropositionalModalFormula
    | Implication !PropositionalModalFormula !PropositionalModalFormula
    | Necessity   !PropositionalModalFormula
    | Possibility !PropositionalModalFormula
    deriving (Eq, Show, Read)