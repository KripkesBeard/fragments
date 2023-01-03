{-|
This is the "internal" module for propositional modal logic semantics. See the main Semantics.Logic.PropositionalModal module 
for an explanation of what is going on.
-}

module Semantics.Logic.PropositionalModal.Semantics 
    ( -- * Model Structure
      -- ** Possible Worlds
      World
    , createWorld
    , Worlds
    , createWorlds
    , -- ** Accessibility Relation
      AccessibilityRelation
    , createAccessibilityRelation
    , -- ** Kripke Frame
      KripkeFrame
    , createKripkeFrame
    , -- ** True Propositions
      TruePropositions
    , createTruePropositions
    , -- ** Interpretation
      Interpretation
    , createInterpretation
    , -- ** Kripke Model
      KripkeModel
    , createKripkeModel
    , -- * Evaluation
      evaluateAtIn
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Semantics.Logic.PropositionalModal.Syntax
  ( PropositionalVariable
  , PropositionalModalFormula (..)
  )


-- | A world is a distinct point with no interal structure.
type World = Int

-- | Create a world.
createWorld :: Int -> World
createWorld = id

-- | Worlds are a set of worlds.
type Worlds = Set World

-- | Create a set of worlds.
createWorlds :: [World] -> Set World
createWorlds = Set.fromList


-- | An accessibility relation is a map from a world to a set of worlds. 
type AccessibilityRelation = Map World (Set World)

-- | Create an accessibility relation.
createAccessibilityRelation :: [(World, Set World)] -> AccessibilityRelation
createAccessibilityRelation = Map.fromList


-- | A Kripke frame is a pair of worlds and an accessibility relation defined on them.
type KripkeFrame = (Set World, AccessibilityRelation)

-- | Create a Kripke frame.
createKripkeFrame :: Set World -> AccessibilityRelation -> KripkeFrame
createKripkeFrame = (,)


-- | A set of propositions true in a world.
type TruePropositions = Set PropositionalVariable

-- | Create a set of true propositions.
createTruePropositions :: [PropositionalVariable] -> TruePropositions
createTruePropositions = Set.fromList 


-- | An interpretation is a mapping from a world to the set of propositions true in that world.
type Interpretation = Map World TruePropositions

-- | Create an interpretation.
createInterpretation :: [(World, TruePropositions)] -> Interpretation
createInterpretation = Map.fromList


-- | A Kripke Model is a pair of a Kripke frame and an interpretation.
type KripkeModel = (KripkeFrame, Interpretation)

-- | Create a Kripke model.
createKripkeModel :: KripkeFrame -> Interpretation -> KripkeModel
createKripkeModel = (,)


-- | Evaluate the truth of a formula at a world according to a Kripke model.
--
-- Let km be a Kripke model with worlds w, w', and w'', an accessibility relation
--
-- @
-- w  -> w'
-- w' -> w'
-- w' -> w''
-- @
--
-- and an interpretation which assigns 
-- 
-- @
-- "a"      -> w
-- "a", "b" -> w'
-- "b", "c" -> w''
-- @
-- 
--
-- then
--
-- >>> evaluateAtIn (Necessity (Disjunction (Atomic 1) (Atomic 3))) w km
-- True
evaluateAtIn :: PropositionalModalFormula -> World -> KripkeModel ->  Bool
evaluateAtIn (Atomic p)        w km = p `Set.member` getPropositions km w
evaluateAtIn (Negation p)      w km = not (evaluateAtIn p w km)
evaluateAtIn (Conjunction p q) w km = evaluateAtIn p w km && evaluateAtIn q w km
evaluateAtIn (Disjunction p q) w km = evaluateAtIn p w km || evaluateAtIn q w km
evaluateAtIn (Implication p q) w km = not (evaluateAtIn p w km) || evaluateAtIn q w km
evaluateAtIn (Necessity p)     w km = Set.fold (&&) True  (mapOverWorlds evaluateAtIn km p (accessWorlds km w))
evaluateAtIn (Possibility p)   w km = Set.fold (||) False (mapOverWorlds evaluateAtIn km p (accessWorlds km w))


-- | Grabs the set of propositions true at a given world in a given kripke model.
--
-- Internal helper function.
getPropositions :: KripkeModel -> World -> Set PropositionalVariable
getPropositions km w = snd km Map.! w

-- | Grabs the accessible worlds from a Kripke model.
--
-- Internal helper function.
accessWorlds :: KripkeModel -> World -> Set World
accessWorlds km w = snd (fst km) Map.! w

-- | Maps a function (with the type signature of the evaluation function) over a set of worlds, turning it into a set of booleans.
-- 
-- Internal helper function.
mapOverWorlds :: (PropositionalModalFormula -> World -> KripkeModel ->  Bool) -> KripkeModel -> PropositionalModalFormula -> Set World -> Set Bool
mapOverWorlds f km p = Set.map (\w -> f p w km)