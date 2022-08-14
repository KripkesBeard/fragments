# Saturated

This is an exploration of formal semantics via functional programming techniques. 
The goal is to create a Haskell library which serves as a basic framework for 
implementing fragments of natural language.

The plan is to move from a basic, extensional fragment of Montague semantics to 
a possible worlds style intensional version, and then explore various different 
approaches to hyperintensionality. One approach will be from the functional 
programming side, which is to use monads and continuations to model various 
intensional/hyperintensional phenomena. The second approach is from the philosophy 
of language, and is the formalization of various approches to the problems, 
such as two dimensional semantics, aboutness, impossible worlds, and structured propositions.

I also want to formalize the meta theory in Agda, which will allow for correctness 
proofs, as well as being able to work with the meta theory in an explicit way. 
Agda also offers the opportunity to use an intuitionistic 
type theory based approach to the formalization process as opposed to the more 
traditional Montagovian approach.

See lit.md for a summary of the relevant literature.

## Structure

| Module | Description |
| ------ | ----------- |
| Logic | The main logics of the project |
| Fragments | Formalizations of fragments/systems of semantics from various books |
| Hyperintensions | Explorations of various approaches to hyperintensionality |
| Functional | Explorations of ideas from functional programming such as recursion schemes to semantics |
| Dependent | Use of dependent type theory and Agda to develop fragments |
| Foundations | Proofs and formalizations in Agda of the metatheory for these systems |

