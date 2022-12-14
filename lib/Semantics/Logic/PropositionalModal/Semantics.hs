module Semantics.Logic.PropositionalModal.Semantics where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Semantics.Logic.PropositionalModal.Syntax
  ( PropositionalVariable
  , PropositionalModalFormula (..)
  )

type World = String

type Worlds = Set World

type AccessibilityRelation = Map World Worlds

type KripkeFrame = (Worlds, AccessibilityRelation)

type Interpretation = Map PropositionalVariable Bool

type ModalInterpretation = Map World Interpretation

type KripkeModel = (KripkeFrame, ModalInterpretation)

evaluateAt :: KripkeModel -> World -> PropositionalModalFormula -> Bool
evaluateAt km w (Atomic p)        = snd km Map.! w Map.! p
evaluateAt km w (Negation p)      = not (evaluateAt km w p)
evaluateAt km w (Conjunction p q) = evaluateAt km w p && evaluateAt km w q
evaluateAt km w (Disjunction p q) = evaluateAt km w p || evaluateAt km w q
evaluateAt km w (Implication p q) = not (evaluateAt km w p) || evaluateAt km w q
evaluateAt km w (Necessity p)     = Set.fold (&&) True  (Set.map (\world -> evaluateAt km world p) $ snd (fst km) Map.! w) 
evaluateAt km w (Possibility p)   = Set.fold (||) False (Set.map (\world -> evaluateAt km world p) $ snd (fst km) Map.! w)