{-|
This is the "internal" module for modal predicate logic syntax. See the main Semantics.Logic.QuantificationalModal module 
for an explanation of what is going on.
-}

module Semantics.Logic.QuantificationalModal.Syntax 
    ( -- * Abstract Syntax
      QuantificationalModalTerm
    , QuantificationalModalFormula (..)
    ) where


-- | A quantificational modal term is the name of a constant.
type QuantificationalModalTerm = String

-- | A quantificational formula is a well formed abstract syntax tree encoding its logical form.
data QuantificationalModalFormula
    = Predicate   String [QuantificationalModalTerm]
    | Equality    QuantificationalModalTerm QuantificationalModalTerm
    | Negation    QuantificationalModalFormula
    | Conjunction QuantificationalModalFormula QuantificationalModalFormula
    | Disjunction QuantificationalModalFormula QuantificationalModalFormula
    | Implication QuantificationalModalFormula QuantificationalModalFormula
    | Necessity   QuantificationalModalFormula
    | Possibility QuantificationalModalTerm
    | Universal   (QuantificationalModalTerm -> QuantificationalModalFormula)
    | Existential (QuantificationalModalTerm -> QuantificationalModalFormula)