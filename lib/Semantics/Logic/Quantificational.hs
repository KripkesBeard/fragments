{-|
= Quantificational Logic

Quantificational logic, also called first-order logic or predicate logic, is the logic which studies subject-predicate pairs and quantification.



== The Syntax of Quantificational Logic

Syntax 

== The Semantics of Quantificational Logic

Semantics


-}

module Semantics.Logic.Quantificational 
    ( -- * Syntax
      -- ** Abstract Syntax
      QuantificationalTerm
    , QuantificationalFormula (..)
      -- * Semantics 
      -- ** Model Structure
      -- *** Domain
    , Domain
    , createDomain
      -- *** Relations
    , Relations
    , createRelations
      -- *** Interpretation
    , Interpretation
    , createInterpretation
      -- ** Evaluation
    , evaluateIn
    ) where

import Semantics.Logic.Quantificational.Syntax
    ( QuantificationalTerm
    , QuantificationalFormula (..)
    )

import Semantics.Logic.Quantificational.Semantics 
    ( Domain
    , createDomain
    , Relations
    , createRelations
    , Interpretation
    , createInterpretation
    , evaluateIn
    )