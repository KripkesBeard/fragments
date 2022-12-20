{-|
This is the "internal" module for predicate logic syntax. See the main Semantics.Logic.Quantificational module 
for an explanation of what is going on.
-}

module Semantics.Logic.Quantificational.Syntax 
    ( -- * Abstract Syntax
      QuantificationalTerm
    , QuantificationalFormula (..)
    ) where

-- | A quantificational term is the name of a constant.
type QuantificationalTerm = String

-- | A quantificational formula is a well formed abstract syntax tree encoding its logical form.
data QuantificationalFormula
    = Predicate   String [QuantificationalTerm]
    | Equality    QuantificationalTerm QuantificationalTerm
    | Negation    QuantificationalFormula
    | Conjunction QuantificationalFormula QuantificationalFormula
    | Disjunction QuantificationalFormula QuantificationalFormula
    | Implication QuantificationalFormula QuantificationalFormula
    | Universal   (QuantificationalTerm -> QuantificationalFormula)
    | Existential (QuantificationalTerm -> QuantificationalFormula)