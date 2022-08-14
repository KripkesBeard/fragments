# Guide to the Literature

- [Guide to the Literature](#guide-to-the-literature)
  - [Formal Semantics](#formal-semantics)
  - [Functional Programming](#functional-programming)
  - [Formal Semantics and Functional Programming](#formal-semantics-and-functional-programming)
  - [Logic](#logic)
  - [Philosophy of Language](#philosophy-of-language)

The idea of using a functional programming language such as Haskell 
as a 'semantic calculator' is an intuitive one, given that Montague
used a lambda calculus as his metatheory.
It is also not unique to this project. 
Below are references to the literature both of formal semantics, using functional programming to implement it, as well 
as various other related topics such as the philosophy of language.

## Formal Semantics

There are various introductions to the field of formal semantics which are 
worth checking out.

* ### [Mathematical Methods in Linguistics](https://link.springer.com/book/10.1007/978-94-009-2213-6) by Partee, et al.

    This is a reference book on all of the mathematical prerequisites 
needed to do formal semantics, including set theory, symbolic
logic, algebra, and ideas from theoretical computer science.
It is a great reference to have. 

* ### [Introduction to Montague Semantics](https://link.springer.com/book/10.1007/978-94-009-9065-4) by Dowty, et al.

    This is a classic introduction to Montague semantics which 
develops step by step an intensional logic and then uses it
to model fragments of natural language.

* ### [Logic Language and Meaning Vols I & II](https://press.uchicago.edu/ucp/books/book/chicago/L/bo3618810.html) by L.T.F. Gamut

    These two books are wide ranging introductions to formal logic
as well as semantics. The author is a pseudonym for a collection
of Dutch logicians who have a particular approach to the 
subject. 

* ### [Semantics in Generative Grammar](https://www.wiley.com/en-us/Semantics+in+Generative+Grammar-p-9780631197133) by Heim & Kratzer

    This is the standard book in formal semantics and (along with the sequel below) 
is one of the main points of reference for this project.

* ### [Intensional Semantics](https://github.com/fintelkai/fintel-heim-intensional-notes) by von Fintel & Heim

    This is the sequel to the Heim & Kratzer book. H&K leaves off at the 
beginnings of intensionality, only sketching how the semantics works. 
This book is an exploration of intensional semantics in the same style.

* ### [Invitation to Formal Semantics](https://eecoppock.info/bootcamp/Invitation_to_formal_semantics-2022Jan18.pdf) by Coppock & Champollion

    This is the second main book that the project looks at. It is a more contemporary 
(the most current on this list) introduction to the field, and is an attempt
to fix some of the more idiosyncratic decisions made in H&K and vF&H.

* ### [Compositional Semantics](https://global.oup.com/academic/product/compositional-semantics-9780199677146) by Jacobson
  
    This is an interesting book which compares and contrasts a direct compositional
approach with an indirect approach using logical form.

## Functional Programming

Functional programming is based on the lambda calculus, so correspondingly there 
are both typed and untyped functional languages. While types form the backbone of 
Montague semantics, untyped functional programming/lambda calculi are still very 
relevant, and Lisp in particular has a lot of interesting features for the 
enterprise of computational semantics. 

* ### [Structure and Interpretation of Computer Programs](https://web.mit.edu/6.001/6.037/sicp.pdf) by Abelson & Sussman

    This is one of if not the greatest book on computer science ever 
written. It is not really a tutorial on Lisp/Scheme, although you'll learn a lot 
of the language by following it, but it is without a doubt the most 
pedagogically sound exposition of functional programming that exists. 
The fourth and fifth section also contain invaluable ideas about language design
and interpretation.

* ### [Introduction to Functional Programming](https://www.google.com/books/edition/Introduction_to_Functional_Programming/Op5QAAAAMAAJ) by Bird & Wadler

    This book is like a more straightforward version of *Structure*, but 
focusing on typed functional programming. There are three versions, the first 
uses a mathematical syntax which is almost Miranda (a precursor to Haskell), 
and the second uses Haskell. The third is a major rewrite, with a different title, 
by the first author, so it might be more apt to call it a spiritual successor. All 
three of them are good, but the first is the best.

For an introduction to Haskell as a language, there are a few choices.

* ### [Get Programming with Haskell](https://www.manning.com/books/get-programming-with-haskell) by Kurt

    This is the best introduction to Haskell as a programming 
language for people who want to start development right away. It is 
well written and covers all of the basics of the language, and I believe it's 
also just in general the easiest introduction to the language to use. 

* ### [Haskell in Depth](https://www.manning.com/books/haskell-in-depth) by Bragilevsky

    This book is a 'second course' in Haskell, designed to show the in 
depth details of using Haskell as a language to write software. It also contains 
some sections discussing more advanced features of the type system. The combination of 
*Get Programming* and this book is all anyone needs to get Haskell proficiency.

* ### [Haskell Programming from First Principles](https://argumatronic.com/) by Allen & Moronuki

    An alternative to using the two books above, this is a more difficult but 
thoroughly comprehensive introduction to the language that for a long time was
the de facto standard recommendation to learn. The main complaint people have 
with the book is its length, and that it can be slow to start because it does 
begin from first principles, but if you look at the book as being in two parts, 
an introduction and an advanced section, it comes out to be shorted than doing 
*Get Programming* followed by *In Depth*. (P.S. dO nOT pirate the book and
send some money [to Julie](https://ko-fi.com/argumatronic) instead of buying it after 
googling the history of the other co-author.)

Haskell is a testbed for advances in programming language theory as well as 
abstract functional programming concepts (like monads). Like monads, some of these
ideas can be used to interesting effect in formal semantics.

* ### [Thinking with Types](https://thinkingwithtypes.com/) by McGuire

    This is an introduction to advanced features of Haskell's 
type system, which become necessary (and fun) when you get to higher levels 
of Haskell programming. Many of the ideas are useful in formal semantics, 
including the brief introduction to dependent types at the end.

* ### [Optics by Example](https://leanpub.com/optics-by-example) by Penner
    Optics are a general framework for what is called 'bidirectional data
accessing', and have grown out of decades of work in making records easier 
to work with in Haskell. Their are still a very active area of research 
and have category theoretical underpinnings, and I hope to be able to use them 
to model aspects of natural language semantics. This book is a great introduction 
to the general ideas and most common types of optics, such as lenses, in Haskell.

* ### [Program Design by Calculation](https://www4.di.uminho.pt/~jno/ps/pdbc.pdf) by Oliveira

    Higher-order functions are the heart of both the lambda calculus and 
formal semantics. Frege and Montague both recognized how important they are to 
handling natural language. Function composition is the most ubiquitous higher-order
function, and the algebra it induces is central to writing elegant functional 
programs. This book builds on a book by Bird and de Moor called 
[Algebra of Programming](https://www.google.com/books/edition/Algebra_of_Programming/P5NQAAAAMAAJ) 
which describes this algebra, and importantly touches on a set of tools called 
recursion schemes. Recursion schemes are a higher-order function which abstracts a
pattern of recursion, allowing you to write very simple, fast programs that do 
complicated recursions. Two recursion schemes, catamorphisms and anamorphisms, as well 
as their composition, hylomorphisms, are related to the homomorphism view of the 
syntax semantics interface, and can be used to generalize the process of semantic 
evaluation.

* ### [Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/) by Milewski

    This is a very famous introduction to category theory through the lens 
of Haskell programming. This is a good book to use as a reference for category 
theory in this project because all of the applications to Haskell it discusses
are really applications to the lambda calculus, which makes them relevant 
to formal semantics by definition.

Unfortunately there does not exist a perfect introduction to Agda, in my 
opinion. However, there are some very good ones. 

* ### [Agda Lecture Notes](https://github.com/jespercockx/agda-lecture-notes) by Cockx

    These are a series of lecture notes by one of the main developers of Agda. They
are very nicely written and take the perspective of introducing Agda and dependent 
type theory to someone who already knows Haskell. The only criticism is that 
they're very short and therefore only cover the tip of Agda.

* ### [Proof = Program](https://www.lix.polytechnique.fr/Labo/Samuel.Mimram/teaching/INF551/) by Mimrim

    This is a comprehensive book that introduces much more than just Agda. It starts 
off by explicating functional programming in the form of the OCaml language, and then 
uses it as a reference to talk about classical logic, lambda calculi, and 
intuitionistic logic before moving to Agda, intuitionistic type theory, 
and Homotopy Type Theory. 

* ### [Programming Language Foundations in Agda](https://plfa.github.io/) by Wadler, et al.

    This book is an interactive Agda file which teaches dependently 
typed programming and theorem proving in Agda through the lens of the foundations of
programming language theory, which, incidentally (a running theme, you might say), 
is composed of logic and the lambda calculus. It also has a section on denotational 
semantics for programming languages which is the programming analog of Montague style 
formal semantics. 

## Formal Semantics and Functional Programming 

There are various books which either introduction formal semantics
via functional programming, or else use techniques from functional
programming and related areas to model various aspects of semantics.

* ### [Computational Semantics with Functional Programming](https://staff.fnwi.uva.nl/d.j.n.vaneijck2/cs/) by van Eijck & Unger
   
    This book is the main touch point of our approach to modeling formal semantics
inside of Haskell. It is both an introduction to formal semantics as well as 
Haskell as a language.

* ### [Enriched Meanings](https://global.oup.com/academic/product/enriched-meanings-9780198847854) by Asudeh & Giorgolo

    This book outlines the use of monads to model natural language phenomena such
as intensionality, which will be a major theme of this project.

* ### [Continuations and Natural Language](https://global.oup.com/academic/product/continuations-and-natural-language-9780199575022) by Barker & Shan

    This book demonstrates how the notion of a continuation (which, coincidentally 
is a kind of monad) can be used to model quantifiers and their scope. It is 
also one of the main references of this project.

* ### [Type Theoretical Grammar](https://global.oup.com/academic/product/type-theoretical-grammar-9780198538578) by Ranta
  
    This book is unique in that it uses a Martin-Lof intuitionistic type theory
as the formal background theory instead of a more traditional Montague-style 
intensional logic. It argues in particular that dependent types can be used 
to model complex phenomena such as donkey-sentences. This will be relevant
to the Agda portion of the project.

There are some web pages which host notes/lectures/code from
various seminars/summer schools teaching the exact same approach
to these topics (using Haskell as the embedding language).

* ### [What Philosophers and Linguists Can Learn From Theoretical Computer Science But Didn't Know To Ask](http://lambda.jimpryor.net/) by Barker & Pryor

* ### [Functional Programming Techniques for Philosophy and Linguistics](http://www.jimpryor.net/teaching/nasslli/index.html) by Barker & Pryor

* ### [Monads and Natural Language](https://github.com/dylnb/esslli2015-monads) by Barker & Bumford

* ### [Lambda: the ultimate syntax-semantics interface](https://okmij.org/ftp/gengo/NASSLLI10/) by Kiselyov & Shan

* ### [Dynamic Semantics](https://github.com/cb125/Dynamics) by Barker

See also the immense wealth of writings on the topic at Kiselyov's 
page [here](https://okmij.org/ftp/gengo/).

Finally, it's worth noting that functional programming is not the only paradigm
that can be, or has been, used to model computational formal semantics, and so it is
worth noting references to various other approaches. 

First, there are approaches which embed the semantics into an already existing, 
general purpose language like Haskell. The most popular way of doing this is 
to use Python. 

* ### [Natural Language Tool Kit](https://www.nltk.org/)
  
  The Natural Language Tool Kit is an extensive library for natural language 
processing in Python. That is, it covers much more than just formal semantics. 
The semantics portion of the library is located in nltk.sem and is mostly 
built around an interface which allows users to write first-order lambda 
statements and then evaluate their truth values according to some model. 
There was a book printed that covered the library, but has since become 
out of date. However, there is a semi up-to-date free version of 
[the book](https://www.nltk.org/book/) on the nltk website. Chapter 10 gives 
an introduction to how to use the semantics portion of the library. 

* ### [Lambda Notebook](https://github.com/rawlins/lambda-notebook) by Rawlins

    One of the major benefits of Python is that it has innate integration with 
Jupyter notebooks, which are interactive visual presentation styles of software that
let you embed code inside of a markdown page and anyone viewing the notebook can 
interact with the code segments in real time. The Lambda Notebook is a formal 
semantics written in Python (similar to nltk.sem) but implemented 
inside of a very visually pleasant Jupyter notebook interface. One of the major 
benefits of these Notebooks is that they can instantly generate graphics based 
on the powerful data analysis tools available in Python, and the authors have 
set the notebooks up such that they display LaTeX trees of the sentences you're 
evaluating. I hope to one day set up a Jupyter Notebook interface for this 
project.

A disadvantage of Python as the host language is that Python is inherently not a
functional language, and so you will be fighting with the language at some point 
or another. Imperative and object oriented aspects of the language cannot be avoided, 
and that just adds an unnecessary layer of complexity to getting everything set up.

On the other hand, Prolog based approaches are nice because definite clause grammars 
use unification as a method of evaluating formal grammars, which is exactly what we 
want our system to do.

* ### [Representation and Inference for Natural Language](https://web.stanford.edu/group/cslipublications/cslipublications/site/1575864967.shtml) by Blackburn & Bos

    This book uses Prolog as the host language and implements a lambda calculus 
using a definite clause grammar. It then focuses on resolution and unification as a 
mechanism for inference in natural language. This is a very well constructed project 
and it would be my second choice after Haskell if I were still choosing a general 
purpose language embedding approach. 

One issue that both Python and Prolog share is that they are not typed languages. 
This means that you need to add a type system to the lambda calculus you're designing, 
and you cannot lean on using the host language's type checker in the same way you can 
in Haskell. [A book review](https://dl.acm.org/doi/abs/10.1162/COLI_r_00103) 
of the van Eijck & Unger book  by Robin Cooper provides a brief but concise 
comparison of these three languages, the problem of types, and why Haskell ultimately 
is the best choice.

However, a second method is possible. Instead of embedding our formal semantics 
inside of a general purpose programming language, we instead can create 
a special purpose language designed specifically to handle semantics. 

* ### [The Lambda Calculator](http://lambdacalculator.com/)

The Lambda Calculator is a graphical interface for constructing and 
computing derivations in a Montague style formal semantics. It's developed 
by the linguistics department at NYU and is also the suggested software to 
use with the Coppock & Champollion book. Unfortunately, there are some 
problems with it. First, the calculator is, as mentioned above, a graphical 
interface and not a language in and of itself. It feels like you're doing 
everything by hand like you would on pen and paper, and the computer just 
checks the derivations for you. Maybe there is merit to this in one way, but
it completely defeats the purpose of the computational aspects of projects 
like these. Secondly, it is not open source. This is a major issue, 
given that the goal of these kinds of projects is to expand the resources 
available to the community. But even if it was open source, the application 
is written in Java. Enough said.

* ### [Grammatical Framework](https://www.grammaticalframework.org/) 

This system is a programming language inspired by languages like Haskell, 
but has been built from the ground up to be a host language used to 
specify formal grammars of natural languages. It has an extensive 
library full of grammars for all kinds of natural languages, and has 
a fairly active community of contributors. There is an 
introductory [book](https://www.grammaticalframework.org/gf-book/) 
written by Ranta as well as a large amount of documentation online. Even 
better, the project is open source and written itself in Haskell. If I had 
to choose a specialized language for this project, I would most 
certainly choose GF.


## Logic

While the introductions to formal semantics cover the amount of logic needed, it is
still good to be intimately familiar with classic first-order logic. 

* ### [Mathematical Logic](https://global.oup.com/academic/product/mathematical-logic-9780198571001) by Chiswell & Hodges

    This is the best introduction to elementary logic that exists. One particularly
salient point to our project is that it treats the evaluation of formulae as a 
recursive tree-walking algorithm. It even has a appendix which relates this idea to
formal semantics and the semantics of programming languages. 

* ### [First-Order Logic](https://www.raymondsmullyan.com/books/first-order-logic/) by Smullyan

    This is one of if not the greatest logic book ever written. Smullyan is a master
and this book is the definition of mathematical elegance. As a result, however, it 
is very terse, and therefore should come as a second reading.

There are two more introductions which are specifically aimed at computer scientists 
and as a result include sections which talk about formalizing the results in Prolog.

* ### [First-Order Logic and Automated Theorem Proving](https://link.springer.com/book/10.1007/978-1-4612-2360-3) by Fitting
  
    This book is interesting because it covers all five of the major kinds of 
proof systems, but in particular it focuses on resolution and tableaux, both of 
which get treatments in Prolog.

* ### [Logic for Applications](https://link.springer.com/book/10.1007/978-1-4612-0649-1) by Nerode & Shore

    This book is fairly different from Fitting's in that it covers much more
than just first-order logic. It also touches on modal logic, intuitionistic logic, 
and set theory. One of the most interesting sections is the section on Prolog, 
because it goes into the theory behind Prolog as a system of logic itself.

Finally, modal logic is incredibly important to the study of the intensional logics
used in Montague semantics. 

* ### [Modal Logic for Philosophers](https://www.cambridge.org/core/books/modal-logic-for-philosophers/F06AF29110ED09BE750EE0C649098193) by Garson

    This is a book intended for philosophers and logicians who care about and 
want to engage with issues in metaphysics, the philosophy of language, semantics, 
and related topics. To this end, it spends a majority of the book on various approaches
to first order modal logic and the open texture it engenders. 

* ### [Modal Logic](https://www.cambridge.org/core/books/modal-logic/F7CDB0A265026BF05EAD1091A47FCF5B) by Blackburn et al.

    This book is written by the Dutch logicians mentioned earlier and is a 
demonstration of their mathematical approach to the subject. It forms an interesting 
counterpart to the above book, as they both focus on entirely different uses and 
aspects of modal logic.

* ### [Types, Tableaux, and Godel's God](http://melvinfitting.org/bookspapers/books.html) by Fitting

    This book starts off by introducing intensional logic using higher order type 
theory, and then uses that system to discuss Godel's ontological argument for the 
existence of god. While the latter part of the book is irrelevant to this project, the 
former is an incredibly lucid approach to intensional logic.

## Philosophy of Language

Formal semantics and the philosophy of language are intimately related, 
Montague was a logician after all, and some of the best work in formal semantics 
has come from philosophers (e.g. David Lewis). Even more so, the bleeding edge of 
issues such as hyperintensionality is still further into philosophy of language 
than formal semantics at the moment. So it is fruitful to be familiar with the field.
First, there are various textbook introductions to the subject.

* ### [The Philosophy of Language](https://www.routledge.com/Philosophy-of-Language-A-Contemporary-Introduction/Lycan/p/book/9781138504585) by Lycan

    This book is a comprehensive overview of the philosophy of language 
which proceeds largely in a historical context. It goes over the beginnings of 
the field with Frege and Russell and continues on through a taxonomy of various 
theories of meaning before moving on to issues in pragmatics and language in 
context.

* ### [The Philosophy of Language](https://press.princeton.edu/books/hardcover/9780691138664/philosophy-of-language) by Soames

    This book similarly proceeds historically, but focuses much more on theories 
of meaning and the various uses of possible worlds and propositions to issues 
related to the intersection of language, epistemology, and metaphysics.

* ### [The Philosophy of Language](https://www.cambridge.org/core/books/philosophy-of-language/5DDAE77AB68E5F9FEA06E1C867B43958) by Szabó & Thompson

    This book touches on the same topics but does so with linguists 
in mind, and so offers a very good perspective for the project undertaken here.

All of these touch on specific papers and books from the literature. Luckily, there
are three great collections of papers in the field which contain many (but not all) 
of the most relevant.

* ### [The Philosophy of Language](https://global.oup.com/ushe/product/the-philosophy-of-language-9780199795154) edited by Martinich & Sosa
 
    As the title suggests, this focuses on the philosophy of language and therefore 
has the foundational papers by philosophers like Frege and Russell. 

* ### [Formal Semantics](https://www.wiley.com/en-us/Formal+Semantics%3A+The+Essential+Readings-p-9780470758182) edited by Portner & Partee

    Once again, as the title suggests, this focuses on formal semantics
and likewise has foundational papers by those like Montague and Partee.

* ### [Semantics](https://global.oup.com/academic/product/semantics-9780195136982) edited by Davis & Gillon

    This book is a mix between the philosophy of language and formal 
semantics camps, and it has slight overlap with the other two books, but is still 
worth checking out.

One of the main contributions I want to make with this project is an exploration 
of hyperintensionality. The issue is that we do not currently have one dominating 
approach to the subject. So here are some representative works from the various
approaches to the problem. The important thing to note is that the received view 
is that standard possible world semantics is not fine grained enough to support
hyperintensionality, and so it either needs to be abandoned or expanded.

* ### [New Thinking about Propositions](https://academic.oup.com/book/6242) by King, et al.

    This book represents the dominant approach to the subject, namely 
structured propositions. It is a collection of essays by three of the main 
theorists of the theory who each give arguments for why it's right, what their 
specific version of it entails, and critiques of the other two versions. 

* ### [Aboutness](https://press.princeton.edu/books/hardcover/9780691144955/aboutness) by Yablo

    This book is a novel approach to the subject, and involves keeping 
possible world semantics, but adding in a new notion of 'aboutness' which tries to carve out more fine grained sections of worlds. 

* ### [Impossible Worlds](https://global.oup.com/academic/product/impossible-worlds-9780198812791) by Berto & Jago

    Impossible worlds are one of the main traditional ways of expanding 
possible world semantics to cover hyperintensionality. It involves changing 
the underlying modal logic so as to include impossible worlds and using 
those to model various linguistic, metaphysical, and epistemic phenomena.

* ### [Two-Dimensional Semantics](https://global.oup.com/academic/product/two-dimensional-semantics-9780199272020) edited by García-Carpintero & Macià

    Two-dimensional semantics is another classic approach to the
issue and has been studied for decades. This is a collection of essays on the 
topic with both arguments for and critiques against.

* ### [Categories and Modalities](https://academic.oup.com/book/35959/chapter-abstract/310544630) by Kishida in [Categories for the Working Philosopher](https://global.oup.com/academic/product/categories-for-the-working-philosopher-9780198748991) edited by Landry 
  This essay explains how monads and functorial semantics can be used to model modal 
notions and will serve as a foundational text for the monadic approach.

* ### [Modal Homotopy Type Theory](https://global.oup.com/academic/product/modal-homotopy-type-theory-9780198853404) by Corfield

    This book takes a radical stand in arguing that traditional logic should be
usurped by homotopy type theory. Because of the use of Agda in this project, it makes 
sense to give ideas from HoTT a chance and hopefully we can learn something, even if 
I am very skeptical of the enterprise.
