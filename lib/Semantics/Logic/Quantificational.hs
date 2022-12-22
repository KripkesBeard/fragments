{-|
= Quantificational Logic

Quantificational logic, also called first-order logic or predicate logic, is the logic which studies subject-predicate pairs and quantification.
This logic allows us to form statements about a domain of objects, their relations to each other, and their relations to the domain as a whole.
It is more expressive than propositional logic because it allows for quantifying over objects in a domain instead of just combining whole sentences via the logical connectives.


== The Syntax of Quantificational Logic

The syntax of quantificational logic requires two separate definitions, terms and formulae.

A term is either a variable or a constant:

\[
\textrm{Term} ::= \: v \: | \: c
\]

where the variables \(v\) are a different kind of variable than a propositional variable. Here, the variables \(v\) range over objects in the domain.

For implementation purposes, however, the Haskell grammar for terms only includes constants. The reason for making this decision will be explained 
in the section on semantics, but the idea is that we want to use Haskell's built in handling of variables to manage the complexities which quantification brings.

A quantificational formula is defined according to the grammar:

\[
\Phi ::= \: P^n(t_1,t_2,...,t_n) \: | \: t_1 = t_2 \: | \: \lnot \: \Phi \: | \: \Phi \land \Phi \: | \: \Phi \lor \Phi \: 
| \: \Phi \rightarrow \Phi \: | \: \forall x \Phi \: | \: \exists x \Phi 
\]

where \(P^n\) is an n-place predicate, \(t_n\) are terms and \(x\) is a variable. A formula conforming to the first clause is atomic and is the quantificational equivalent of a 
propositional atomic sentence if the terms are constants. The quantifying symbols \(\forall\) and \(\exists\) are repsectively known as the universal quantifier 
and the existnetial quantifier, with the first meaning "for all" and the second "there exists".

According to the grammar, there are two kinds of new things happening compared to propositional logic. First, instead of an indivisible propositional formula letter, we break the
proposition down into a predicate and the objects it predicates. We can also express equality between two objects in the domain. Syntactically, equality is really just a two place 
predicate, but it has some special semantic properties so we include it in the grammar. Second, we have the introduction of the quantifying symbols \(\forall\) and \(\exists\). Both 
of them take a variable and a formula to produce a new formula. 

The variable which appears next to the quantifying symbol is said to be "bound" by the quantifier, along with all occurances of the variable in the formula
following the quantifier. The intuitive idea is that we want to treat a quantified variable as if the name itself doesn't matter. \(\forall x\) 
and \(\forall y\) really have the same meaning, we're quantifying over some variable, not a particular object. The only reason we have a 
different variable name is so that we can easily tell which variable is bound by which quantifier in a case like \(\forall x \exists y\). A formula is said to be 
open if it contains any variable which is not bound by some quantifier. A formula is said to be closed if all of its variables are bound, or if it only 
contains constants. We also sometimes use the word "sentence" to mean a closed formula.

Dealing with unbound variables is the most annoying part of any logic, and so the reason that the implementation does not consider variables as part of 
the AST grammar is to unload that problem onto Haskell itself. All of this will make sense once the interpretation of a formula is described,
but the idea behind the implementation is to make open formulae impossible to represent.

The last syntactic idea which needs to be covered is substitution. In a formula which contains variables, we can substitute a term for each occurance of a 
variable by simply replacing each occurance of that variable with the term.  

Substituting the constant \(c_1\) for the variable \(x_1\) in the formula \(\lnot P_1(x_1,c_2) \land P_2(x_1)\) results in the 
formula \(\lnot P_1(c_1,c_2) \land P_2(c_1)\).


== The Semantics of Quantificational Logic

The semantics of quantificational logic goes beyond propositional logic in that instead of just mapping sentence to true and false, we want to 
be able to describe objects in a domain of discourse and relations among them. So, an interpretation \(I\) is a pair \(\langle D, R \rangle\)
where \(D\) is the domain of the interpretation and \(R\) is a set of relations on \(D\). That is, \(D\) is a set of objects which the formulae 
of the language talk about, and \(R\) is a collection of n-tuples on \(D\). An interpretation assigns one of these n-tuples to each predicate 
in order to tell exactly which objects in the domain the predicate is true of. It also assigns an object in the domain to each constant, which can also be understood as 
assigning a one place predicate true of exactly one object in the domain. 


The recursive truth definition for a quantificational sentence (closed formula) in some interpretation \(I\) is:

\[
\textrm{An atomic sentence} \: \ulcorner P^n(t_1,t_2,...,t_n) \urcorner \: 
\textrm{is true iff the n-tuple containing the objects in \(D\) which \(I\) assigns to \(\ulcorner t_1 \urcorner , \ulcorner t_2 \urcorner,..., \ulcorner t_n \urcorner\) 
is in the relation which \(I\) assigns to \(\ulcorner P^n \urcorner\)}
\]

\[
\ulcorner t_1 = t_2 \urcorner \: \textrm{is true iff} \: I \: \textrm{assigns \(\ulcorner t_1 \urcorner\) and \(\ulcorner t_2 \urcorner\) the same object in \(D\)} 
\] 

\[
\ulcorner\lnot \: \phi\urcorner \: \textrm{is true iff} \: \ulcorner\phi\urcorner \: \textrm{is false}
\]

\[
\ulcorner\phi \: \land \: \psi\urcorner \: \textrm{is true iff} \: \ulcorner\phi\urcorner \: \textrm{is true and} \: \ulcorner\psi\urcorner \: \textrm{is true}
\]

\[
\ulcorner\phi \: \lor \: \psi\urcorner \: \textrm{is true iff} \: \ulcorner\phi\urcorner \: \textrm{is true or} \: \ulcorner\psi\urcorner \: \textrm{is true}
\]

\[
\ulcorner\phi \: \rightarrow \: \psi\urcorner \: \textrm{is true iff either} \: \ulcorner\lnot \: \phi\urcorner  \: 
\textrm{is true or} \: \ulcorner\psi\urcorner \: \textrm{is true}
\]

\[
\ulcorner \forall x \phi \urcorner \: \textrm{is true iff the formula \(\ulcorner \phi \urcorner\) is true for every object \(d \in D\) when every occurance of \(x\) in \(\phi\) is 
replaced with a term denoting \(d\)}
\]

\[
\ulcorner \exists x \phi \urcorner \: \textrm{is true iff the formula \(\ulcorner \phi \urcorner\) is true for at least one object \(d \in D\) when every occurance of \(x\) in \(\phi\) is replaced with a term denoting \(d\)}
\]


Again, the differences between this definition and the definition for propositional logic have to do with the predicate clause, the equality clause, and the quantifier clauses. The 
predicate clause explains that a predicate is true of its terms iff the n-tuple of objects denoted by those terms are in the relation which \(I\) assigns to the predicate. The idea that 
a sentence \(\ulcorner t_1 = t_2 \urcorner\) is true just in case the terms denote the same object is sometimes called extensionality. In modal and intensional logics, the way in which 
an object is designated can matter greatly to the question of equality, and so the clause for \(\ulcorner t_1 = t_2 \urcorner\) might not hold in those logics. Here, however, we only 
care about extensional equality. The quantifiers clauses formalize the intuitive idea that a universally quantified sentence is only true if it's true of every object in the domain, and 
an existenetially quantified sentence is similarly only true if its true of at least one object in the domain.

It's important to understand that asking whether or not a formula with unbound variables is true makes no sense. It depends on what the variable stands for, and without any sort of 
quantifying, there isn't a coherent way to figure that out. This is why we only consider sentences and not open formulae in the truth conditions. 

In general, there are at least three common ways of dealing with the problem of open formulae and they all involve binding or replacing the unbound variables. The first way is to just 
agree that we will only ever try to interpret a closed formula. The second way is to treat every occurance of an unbound variable as implicitly being bound by one of 
the quantifiers (usually \(\forall\)). The third way is to include another parameter in our evaluation process which is usually referred to as an "assignment function" which 
takes a variable and assigns it a constant name referring to some object in the domain. The effect of the second and third options is that before we evaluate the formula, we rewrite 
it as a closed formula by either binding all of the unbound variables with a universal quantifier or reassigning all of the free variables according to the assignment function.

Option three is very common, but it's extra work. Option two is fine, but it can be somewhat unintuitive if you don't see a bias towards universal quantification. Option 
one is what this implementation chooses, and it does so by making unbound variables unrepresentable. A variable can only appear in the scope of a quantifier, because the 
quantifiers are written using Haskell's own variables. Terms in the implementation are always constants, so there cannot be a syntactically correct string containing an 
unbound variable. At some point during the evaluation process, the formula needs to become closed. All three ways outlined above are perfectly acceptable, but leveraging 
Haskell's own variables allows for removing a lot of the headache of managing scope.

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