module Semantics.Logic.Quantificational.Semantics
    ( -- * Model Structure
      -- ** Domain
      Domain
    , createDomain
      -- ** Relations
    , RelationalTuples 
    , createRelationalTuple
    , Relations
    , createRelations
      -- ** Interpretation
    , Interpretation
    , createInterpretation
      -- * Evaluation
    , evaluateIn
    ) where

import Semantics.Logic.Quantificational.Syntax
    ( QuantificationalTerm
    , QuantificationalFormula (..)
    )

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


-- | A domain is a set of entities.
type Domain = Set String

-- | Create a domain.
createDomain :: [String] -> Domain
createDomain = Set.fromList


-- | A relational tuple is a set of lists of quantificational terms
type RelationalTuples = Set [QuantificationalTerm]

-- | Create a RelationalTuple
createRelationalTuple :: [[QuantificationalTerm]] -> RelationalTuples
createRelationalTuple = Set.fromList 


-- | Relations are the collections of objects satisfying them.
type Relations = Map String RelationalTuples

-- | Create a collection of relations.
createRelations:: [(String, RelationalTuples)] -> Relations
createRelations = Map.fromList


-- | An interpretation is a domain and a collection of relations on it.
type Interpretation = (Domain, Relations)

-- | Create an interpretation.
createInterpretation :: Domain -> Relations -> Interpretation
createInterpretation = (,)


-- | Evaluate the truth of a closed formula in an interpretation.
--
-- Let i be an interpretation with domain Laura, BOB, and Cooper,
-- and relations
--
-- @
-- InTheBlackLodge    = {Laura, BOB}
-- InGlastonburyGrove = {Cooper}
-- @
--
-- then 
--
-- >>> evaluateIn (Universal $ \x -> (Disjunction (Predicate "InTheBlackLodge" [x]) (Equality x "Cooper"))) i
-- True
--
evaluateIn :: QuantificationalFormula -> Interpretation -> Bool
evaluateIn (Predicate f n)   i = (f `Map.member` snd i) && (n `Set.member` (snd i Map.! f))
evaluateIn (Equality t1 t2)  i = (t1 `Set.member` fst i && t2 `Set.member` fst i) && (t1 == t2)
evaluateIn (Negation p)      i = not (evaluateIn p i)
evaluateIn (Conjunction p q) i = evaluateIn p i && evaluateIn q i
evaluateIn (Disjunction p q) i = evaluateIn p i || evaluateIn q i
evaluateIn (Implication p q) i = not (evaluateIn p i) || evaluateIn q i
evaluateIn (Universal u)     i = and (fmap ((`evaluateIn` i) . u) (Set.toList $ fst i))
evaluateIn (Existential u)   i = or  (fmap ((`evaluateIn` i) . u) (Set.toList $ fst i))