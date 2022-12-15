{-|
Module : Semantics.Logic.Propositional.Syntax

= Propositional Logic

Propositional logic, or Boolean logic, is the logic of individual sentences and their compounds. Propositional logic is one of the most basic 
logics which still exhibits interesting behavior. One of the benefits of studying propositional logic is that its syntax and semantics are very 
simple, so the general ideas and techniques used to define these two aspects of a languae can be fully grasped before moving on to much more complicated logics.

== The Syntax of Propositional Logic

The syntax of propositional logic describes how to write down formulae of the language, that is, how to form grammatically correct sentences.

We start with the notion of a propositional sentence letter or variable, often denoted by \(\textrm{A,B,C,...}\). Propositional variables represent the atomic units of 
the language which cannot be further analyzed. Propositional formulae are built up out of propositional letters and other propositional formulae via the logical operations 
of negation, conjunction, disjunction, and implication. That is, a syntactically valid propositional formula \(\Phi\) is recursively constructed according to the grammar: 

\[ 
\Phi := v \: | \: \lnot \: \Phi \: | \: \Phi \land \Phi \: | \: \Phi \lor \Phi \: | \: \Phi \rightarrow \Phi \: 
\]

where \(v\) is a propositional variable. 

This grammar defines an abstract syntax for propositional formulae which can be visualized as a tree. The logical form of a given formula can be computed by simply 
walking the tree and noting which connective appears at each node. The "main connective" of any given tree (or sub-tree) is the connective at the root note of the (sub)tree.

== The Semantics of Propositional Logic

The semantics of propositional logic involves assigning meaning to the syntactic terms of the language. Since propositional logic is the logic of whole sentences, we take the 
meaning of a propositional formula to be the meaning of a sentence, that is, a truth value. A structure which gives meaning to a language is called a model or an interpretation of the 
language. For propositional logic, an interpretation \(I\) is an assignment of true or false to each propositional variable. Then, we define the meaning of compound formulae build up 
out of the connectives with a recursive definition which matches the intuitive meaning of each connective:

\[
\ulcorner\lnot \: \Phi\urcorner \: \textrm{is true iff} \: \ulcorner\Phi\urcorner \: \textrm{is false}
\]

\[
\ulcorner\Phi \: \land \: \Psi\urcorner \: \textrm{is true iff} \: \ulcorner\Phi\urcorner \: \textrm{is true and} \: \ulcorner\Psi\urcorner \: \textrm{is true}
\]

\[
\ulcorner\Phi \: \lor \: \Psi\urcorner \: \textrm{is true iff} \: \ulcorner\Phi\urcorner \: \textrm{is true or} \: \ulcorner\Psi\urcorner \: \textrm{is true}
\]

\[
\ulcorner\Phi \: \rightarrow \: \Psi\urcorner \: \textrm{is true iff whenever} \: \ulcorner\Phi\urcorner \: \textrm{is true, then} \: \ulcorner\Psi\urcorner \: \textrm{is not false}
\]

where \(\Phi\) and \(\Psi\) are propositional formulae, and the corner quotes \(\ulcorner\cdot\urcorner\) represent quasi-quotation. 

What this definition means is that if we have some interpretation \(I\) and some propositional formula \(\Phi\), we can evaluate the formula and find its truth value
by recursively applying these rules until we reach the propositional variables at the ends of the tree and then use the truth value of those variables according to \(I\) to work our way 
back up the tree combining to a final value.

A more computer science-y way of thinking about an interpretation which can be very helpful is to think of it as an enviroment which keeps track of the value of every free variable in 
its scope. Then, the evaluation function just looks up what those values are and uses them as well as the recursive truth rules for the connectives to build the final truth value of the 
sentence.
-}

module Semantics.Logic.Propositional 
    ( -- * Syntax
      -- ** Abstract Syntax
      PropositionalVariable
    , PropositionalFormula (..)
    , -- * Semantics
      -- ** Model Structure
      Interpretation
    , createInterpretation
    , -- ** Evaluation
      evaluateIn
    ) where

import Semantics.Logic.Propositional.Syntax
    ( PropositionalVariable
    , PropositionalFormula (..)
    )

import Semantics.Logic.Propositional.Semantics
  ( Interpretation
  , createInterpretation  
  , evaluateIn
  )