module Semantics.Logic.Propositional.Syntax
    ( PropositionalVariable 
    , PropositionalFormula (..)
    , (/\)
    , (\/)
    , (~>)
    ) where

type PropositionalVariable = String

data PropositionalFormula
    = Atomic      !PropositionalVariable
    | Negation    !PropositionalFormula
    | Conjunction !PropositionalFormula !PropositionalFormula
    | Disjunction !PropositionalFormula !PropositionalFormula
    | Implication !PropositionalFormula !PropositionalFormula
    deriving (Eq, Show, Read)

(/\),(\/),(~>) :: PropositionalFormula -> PropositionalFormula -> PropositionalFormula
p /\ q = Conjunction p q
p \/ q = Disjunction p q
p ~> q = Implication p q

infixr 3 /\
infixr 2 \/
infixr 1 ~>