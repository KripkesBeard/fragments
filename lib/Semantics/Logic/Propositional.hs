module Semantics.Logic.Propositional 
    ( PropositionalVariable
    , PropositionalFormula (..) -- For the sake of abstraction, once I finish the interactive stuff, don't export the constructors
    , (/\)
    , (\/)
    , (~>)
    , Interpretation
    , makeInterpretation
    , evaluateAt
    , pPrint
    ) where

import Semantics.Logic.Propositional.Syntax
    ( PropositionalVariable
    , PropositionalFormula (..)
    , (/\)
    , (\/)
    , (~>)
    )

import Semantics.Logic.Propositional.Semantics
  ( Interpretation  
  , makeInterpretation
  , evaluateAt
  )

import Semantics.Logic.Propositional.Interactive 
  ( pPrint
  )