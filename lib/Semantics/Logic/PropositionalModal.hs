{-|
= Propositional Modal Logic

Where traditional logic studies what is true, modal logic studies in what ways things are true. Propositional modal logic is 
an extension of propositional logic which adds two operators, the box and the diamond, which represent the concepts of
necessity and possibility. That is, instead of just saying that a sentence is true, we can now say that a sentence is necessarily true. 
The good news is that the changes to the syntax needed to add these operators is very minor. On the other hand, the changes to the 
semantics seem to require a very sophisticated new apparatus. In actuality, however, the new semantics is really just a generalization 
of propositional semantics and seems utterly natural in light of the concept of a possible world.

== The Syntax of Propositional Modal Logic

The only change we need to make to the syntax of propositional logic is the addition of the two new operators which, similarly to the
negation operator, take one propositional formula as an argument. The recursive grammar for a propositional modal formula is:

\[ 
\Phi := v \: | \: \lnot \: \Phi \: | \: \Phi \land \Phi \: | 
\: \Phi \lor \Phi \: | \: \Phi \rightarrow \Phi \: | \: \Box \: \Phi \: | \: \Diamond \: \Phi  
\]

where once again \(v\) is a propositional variable.

== The Semantics of Propositional Modal Logic

The interpretation of \(\Box\) and \(\Diamond\) are, respectively, necessity and possibility. That is, \(\Box \textrm{A}\) means that \(\textrm{A}\) 
is not only true, but is necessarily true, or in other words, could not have possibly been false. Similarly, \(\Diamond \textrm{A}\) means that 
it is possible that \(\textrm{A}\) is true, or in other words, \(\textrm{A}\) does not necessarily have to be false. This dual definition hints at 
the fact that each one of the operators could be defined in terms of the other and negation.

The key idea behind modal semantics is the concept of a possible world. In this case, a world doesn't mean the Earth in particular. A possible world is a 
complete description of the way that reality could be. The easiest way to come up with an example of a possible world is to think of a single moment in time.
At any given moment, the entire universe exists in a certain state, and we could describe every fact about the universe with its own propositional letter.
That is, the proposition that "Tom is in his house" is either true or false at any given point in time, and the same is true of all other propositional sentences.
So, a possible world is a complete description of the way that reality might be. Or equivalently, it's a collection of all of the true and false statements which 
describe the world. Possible worlds give an elegant way of describing necessity and possibility. A proposition is necessary if it is true at every possible world, and it is possible
if it is true at some possible world. 
    
Except the real story is slightly more complicated than that, but not very. The first step towards a semantics for propositional modal logic is to define a Kripke frame.
A Kripke frame is a pair \(\langle W, R \rangle\) containing a set of possible worlds \(W\) and a relation \(R \subseteq W \times W\) on that set, usually called an accessibility 
relation. The idea behind the accessibility relation is that given any world, some worlds will be accessible from it while others will not. There are some obvious philosophical 
interpretations of accessibility, but from a mathematical point of view the accessibility relation exists to constrain which worlds we need to check when evaluating whether a statement 
is necessary or possible. 

A Kripke model is a triple \(\langle W, R, I \rangle\) which adds an interpretation function \(I\) to a Kripke frame. The interpretation function takes a world \(w \in W\) and 
assigns to it a set of propositional variables. The interpretation is that those are the propositions which are true at that world. We could instead have each world contain each 
proposition and the interpretation function assign true or false to each proposition at each world, however doing it the former way is easier for the purposes of implementation. 
They're equivalent, but in this case telling whether a proposition is true or false reduces to the problem of checking whether it's in a set. Creating the semantic content of 
a possible world this way only requires assigning which propositions are true, not which propositions are true and which are false.

When evaluating the truth of a modal formula, we evaluate it not just in a Kripke model, but also at a specific world \(w \in W\). This means that we don't just speak of a 
proposition being true, necessarily true, or possible true. We speak of it as being true, necessarily true, or possibly true, at a world. This means that a specific world 
needs to also be an input into our evaluation function.

The truth definition for a propositional modal formula in a Kripke model \(M = \langle W, R, I \rangle\) at a world \(w \in W\) is the following:

\[
\textrm{An atomic formula} \:\ulcorner\phi\urcorner \: \textrm{is true at} \: w \: \textrm{iff} \: I \: \textrm{assigns} \: \ulcorner\phi\urcorner \: \textrm{true at} \: w
\]

\[
\ulcorner\lnot \: \phi\urcorner \: \textrm{is true at} \: w \: \textrm{iff} \: \ulcorner\phi\urcorner \: \textrm{is false at} \: w
\]

\[
\ulcorner\phi \: \land \: \psi\urcorner \: \textrm{is true at} \: w \: \textrm{iff}  \: \ulcorner\phi\urcorner \: \textrm{is true and} \: \ulcorner\psi\urcorner \: \textrm{is true at } \: w
\]

\[
\ulcorner\phi \: \lor \: \psi\urcorner \: \textrm{is true at} \: w \: \textrm{iff}  \: \ulcorner\phi\urcorner \: \textrm{is true or} \: \ulcorner\psi\urcorner \: \textrm{is true at} \: w
\]

\[
\ulcorner\phi \: \rightarrow \: \psi\urcorner \: \textrm{is true at} \: w \: \textrm{iff either} \: \ulcorner\lnot \: \phi\urcorner  \: 
\textrm{is true or} \: \ulcorner\psi\urcorner \: \textrm{is true at } \: w
\]

\[
\ulcorner\Box \: \phi\urcorner \: \textrm{is true at} \: w \: \textrm{iff for all}  \: w' \in W \: \textrm{such that} \: wRw', \: \ulcorner\phi\urcorner \: \textrm{is true at} \: w'
\]

\[
\ulcorner\Diamond \: \phi\urcorner \: \textrm{is true at} \: w \: \textrm{iff there exists a}  \: w' \in W \: \textrm{such that} \: wRw' \: \textrm{and} \: \ulcorner\phi\urcorner \: 
\textrm{is true at} \: w'
\]

What the necessity and possibility clauses say is that in order for a statement to be necessary at a possible world, it needs to be true at every world accessible by
that world. Similarly, a statement is possible at a world if there is at least one world accessed by it in which the statement is true.

One interesting thing to note about these definitions for necessity and possibility is that if we have a world which does not access any other world, then every 
proposition is necessarily true and every proposition is possibly false, in that world. The reason is because since the set of worlds accessed by the world is empty,
the condition for the necessity operator is vacuously true, whereas the condition for the possibility operator has to be false because there does not exist a single
accessed world where the proposition is true. In most philosophical applications, it seems natural for each world to at least access itself, and so that situation would never arise.
mathematically, however, there's nothing that says the accessibility relation needs to be constrained in any way.

It may now be easy to see why propositional modal semantics are a generalization of propositional semantics. Each world is its own propositional model, and the accessibility 
relation allows us to move from one of those models to the next. The truth of a necessity or a possibility is then evaluated by quantifying over the model 
at each of those accessible worlds.
-}

module Semantics.Logic.PropositionalModal 
    ( -- * Syntax
      -- ** Abstract Syntax
      PropositionalVariable
    , PropositionalModalFormula (..)
    , -- * Semantics
      -- ** Model Structure
      -- *** Possible Worlds
      World
    , createWorld
    , createWorlds
    , -- *** Accessibility Relation
      AccessibilityRelation
    , createAccessibilityRelation
    , -- *** Kripke Frame
      KripkeFrame
    , createKripkeFrame
    , -- *** True Propositions
      TruePropositions
    , createTruePropositions
    , -- *** Interpretation
      Interpretation
    , createInterpretation
    , -- *** Kripke Model
      KripkeModel
    , createKripkeModel
    , -- ** Evaluation
      evaluateAtIn
    ) where

import Semantics.Logic.PropositionalModal.Syntax
    ( PropositionalVariable 
    , PropositionalModalFormula(..)
    )

import Semantics.Logic.PropositionalModal.Semantics 
    ( World
    , createWorld
    , createWorlds
    , AccessibilityRelation
    , createAccessibilityRelation
    , KripkeFrame
    , createKripkeFrame
    , TruePropositions
    , createTruePropositions
    , Interpretation
    , createInterpretation
    , KripkeModel
    , createKripkeModel
    , evaluateAtIn
    )