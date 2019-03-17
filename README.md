metasplain - Explainable machine learning
=========================================

metasplain is a suite of tools to explain logic theories learned by the symbolic
machine learning system [Metagol](https://github.com/metagol/metagol) that might
include _invented predicates_ with automatically assigned names. 

metasplain has an automatic mode and an interactive mode. An example of an
interactive session follows:

```prolog
?- _Ps = [(great_grandparent(A,B):- great_grandparent_1(A,C), great_grandparent_2(C,B)), 
         (great_grandparent_1(A,B):- father(A,B)), 
	 (great_grandparent_1(A,B):- mother(A,B)),
         (great_grandparent_2(A,B):- great_grandparent_3(A,B)), 
         (great_grandparent_2(A,B):- great_grandparent_4(A,B)), 
         (great_grandparent_3(A,B):- father(A,C), great_grandparent_1(C,B)), 
         (great_grandparent_4(A,B):- mother(A,C), great_grandparent_1(C,B))].
true.

?- invention_explanation($_Ps,_Es), interpretation(_Es,_Is), 
   writeln('Program with explanations:'), print_programs(_,[_Es]),  
   writeln('Interpreted program:'), print_programs(_,[_Is]).

How should I explain father_or_mother?
|: parent
I will explain father_or_mother as parent

How should I explain father_of_parent?
|: grandfather
I will explain father_of_parent as grandfather

How should I explain mother_of_parent?
|: grandmother
I will explain mother_of_parent as grandmother

How should I explain grandfather_or_grandmother?
|: grand parent
I will explain grandfather_or_grandmother as grand_parent

Program with explanations:
great_grandparent(A,B):-parent(A,C),grand_parent(C,B).
parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).
grand_parent(A,B):-grandfather(A,B).
grand_parent(A,B):-grandmother(A,B).
grandfather(A,B):-father(A,C),parent(C,B).
grandmother(A,B):-mother(A,C),parent(C,B).

Interpreted program:
the great_grandparent is the parent of the grand_parent.
the parent is the father or mother.
the grand_parent is the grandfather or grandmother.
the grandfather is the father of the parent.
the grandmother is the mother of the parent.

true.
```

In the example above, the user is asked to suggest alternatives to the automatic
predicate explanations initially formed by metasplain. The explained program is
printed out, then its interpretation in a Controlled Natural Language.

Note that the order of clauses is preserved in the explained program. This
ensures that the semantics of the learned program are not disturbed by the
explanation process.

An automatic session assigns new names to invented predicates according to the
names of body literals in their clauses without user input:

```prolog
?- _Ps = [(grandparent(A,B):-grandparent_1(A,C),grandparent_1(C,B)),
         (grandparent_1(A,B):-mother(A,B)),
         (grandparent_1(A,B):-father(A,B))].
true.

?- invention_explanation($_Ps,_Es), interpretation(_Es,_Is), 
   writeln('Program with explanations:'), print_programs(_,[_Es]),  
   writeln('Interpreted program:'), print_programs(_,[_Is]).

Program with explanations:
grandparent(A,B):-mother_or_father(A,C),mother_or_father(C,B).
mother_or_father(A,B):-mother(A,B).
mother_or_father(A,B):-father(A,B).

Interpreted program:
the grandparent is the mother_or_father of the mother_or_father.
the mother_or_father is the mother or father.

true.
```

Automatic explanations can get too long and complex for larger programs- in that
case, an interactive session is recommended.

metasplain can swing the other way too, and reconstruct a program from a CNL
interpretation. The following is an example of a session including such a
reconstruction (and otherwise identical to the first example): 

```prolog
?- invention_explanation($_Ps,_Es), 
   interpretation(_Es,_Is), 
   interpretation(_Ps_2,_Is), 
   writeln('Program with explanations:'), print_programs(_,[_Es]),  
   writeln('Interpreted program:'), print_programs(_,[_Is]),
   writeln('Reconstructed Program:'), print_programs(_,[_Ps_2]).

% ... interactive session ...

Interpreted program (with arguments):
A is the great_grandparent of B if A is the parent of C and C is the grand_parent of B.
A is the parent of B if A is the father of B or C is the parent of D if C is the mother of D.
A is the grand_parent of B if A is the grandfather of B or C is the grand_parent of D if C is the grandmother of D.
A is the grandfather of B if A is the father of C and C is the parent of B.
A is the grandmother of B if A is the mother of C and C is the parent of B.

Reconstructed Program:
great_grandparent(A,B):-parent(A,C),grand_parent(C,B).
parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).
grand_parent(A,B):-grandfather(A,B).
grand_parent(A,B):-grandmother(A,B).
grandfather(A,B):-father(A,C),parent(C,B).
grandmother(A,B):-mother(A,C),parent(C,B).

true ;
false.
```

Program reconstruction requires the CNL interpretation to include predicates'
arguments. metasplain can generate such interpretations itself, as in the above
example.

Introduction
------------

Predicates invented by [Metagol](https://github.com/metagol/metagol) in the
course of learning a hypothesis have symbols formed by indexing the symbol of
the learning target with a mix of numbers and underscores- e.g.
deep_learning_pope_1/2, platypus_engineer_2_2_1_4/3, etc. A preponderance of
invented predicates, especially invented predicates calling each other, may
result in a learned hypothesis that is hard to read. Replacing invented symbols
with meaningful names can help alleviate this difficulty.

One way to determine what names it is meaningful to assign to invented
predicates is to elicit domain knowledge from the user. This is the approach
taken by metasplain, where this knowledge takes the form of natural language
expressions used to create an English sentence from the predicate symbols of
body literals in all clauses of an invented predicate.

In the following example, the invented predicate grand_parent_1/2 is renamed to
"father_of_mother/2", by use of the expression "of" to connect the predicate
symbols of the two body literals in its single clause, father/2 and mother/2:

```prolog
% Invented predicate
grand_parent_1(A,B) :- father(A,C), mother(C,B).

% Explained invented predicate
father_of_mother(A,B) :- father(A,C), mother(C,B).
```

The connecting expression "of" was provided by the user, as an _infix operator_
associated with the _chain metarule_. It was chosen to form an explanation of
the single clause of grand_parent_1/2 because that clause matches the chain
metarule:

```prolog
% The chain metarule
P(A,B) :- Q(A,C), R(C,B)
```

Metarules are second-order axioms that define the structure of clauses in
programs learned by Metagol. Each metarule represents a relation over the set of
first order predicates in Metagol's background knowledge. For instance, the
chain metarule represents _transitivity_. Explanation operators provide a
natural language explanation of those second-order relations between predicates.

When a predicate has more than one clause these are combined together using an
_explanation connective_ (usually "or" given that a set of clauses is a
disjunction):

```prolog
father_of_father_or_father_of_mother(A,B):-father(A,C),father(C,B).
father_of_father_or_father_of_mother(A,B):-father(A,C),mother(C,B).
```

Automatically generating explanations in this way can produce overly-long and
still hard-to-read predicate names, as should already be evident from the short
example above. In this case, the user may start an interactive session and
provide human-readable explanations herself. metasplain ensures that the most
basic definitions, that are easier for the user to understand, and so explain,
are presented first. A CNL interpretation of the program can further help the
user get to grips with the results of the interaction. Finally, the user can
test different interpretations by asking metasplain to reconstruct a program
from CNL interpretations.

The end result is that the user and the system build together a common language
of meaningful predicate names with the minimum possible cognitive load placed on
the user in the process.

Instructions of use
-------------------

### Requirements

metasplain requires Swi-Prolog, v.7.0 or higher.

### Loading metasplain.

To begin using metasplain, consult the source file load.pl, in metasplain's top
directory. This will start Swi-Prolog's documentation browser and navigate to
this README file. It will also open metasplain's source files in the Swi-Prolog
IDE. 

Alternatively, metasplain can be started in "headless" mode by consulting
load_headless.pl instead. This will only consult the project's source files,
without starting the documentation server, or the IDE.

### Running metasplain.

metasplain's behaviour is controlled by editing its configuration file,
configuration.pl. Configuration options are explained in the next section.

Once configuration options are set, there are two main predicates to call:

1. invention_explanation/2

   Takes as argument a program learned by Metagol: a list of definite datalog
   clauses that may include invented literals. It outputs the same program with
   predicate names replaced with meaningful names.

2. interpretation/2

   Translates between a program and its a CNL interpretation. The predicate can
   be run with either the program or the explanation as its input (and the other
   one as output).

### Configuration options

The most important configuration options are discussed here. For more details
see the source documentation of the configuration.pl module.

#### interactive_session(?Bool)

This option controls the interaction mode: _automatic_ or _interactive_. Setting
it to "true" enables the interactive mode, setting it to "false" disables it.

In automatic mode, the system forms predicate explanations according to the
given explanations operators and connectives. In interactive mode it pauses
after forming an explanation to ask the user for an alternative explanation.

In interactive mode, input is terminated by a newline. A single return input
accepts an automatically formed explanation:

```prolog
% Set in configuration file: 
interactive_session(true).

% .. Interactive session ...
How should I explain grandfather_or_grandmother?
|: 
I will explain grandfather_or_grandmother as grandfather_or_grandmother
```

It the input cannot be used to form a valid Prolog constant without surrounding
it with quotes, metasplain prompts for an alternative:

```prolog
How should I explain father_or_mother?
|: _X
Sorry, that's not a valid name.

How should I explain father_or_mother?
|:
```

The interactive mode is most useful when automatic explanations may result in
overly long names. In the example at the start of this document, the predicate
interpreted as "grand_parent" by the user would be automatically explained as
"father_of_father_or_mother_or_mother_of_father_or_mother".

#### merge_interpretations(?Bool)

Setting this to "true" instructs metasplain to merge explanations to form more
natural-sounding interpretations:

```prolog
% merge_interpretations(true).

% Interpreted program:
the grand_father is the father of the father or mother.

% merge_interpretations(false).
Interpreted program:
the grand_father is the father of the father or the grand_father is the father of the mother.
```

#### variable_interpretation(?Bool)

Determines whether to include variables in the CNL interpretation of a program.

```prolog
% variable_interpretation(false).
% merge_interpretations(true).

Interpreted program:
the grandfather is the father of the father or mother.

% variable_interpretation(true).
% merge_interpretations(true).

Interpreted program:
A is the grandfather of B if A is the father of C and C is the father of B or mother of B.
```

Variable interpretation without merged interpretations is currently necessary to
allow re-constructing a program from an interpretation. In the future, this
requirement will be lifted.

### Configuring explanation operators and connectives

In this section, we explain the use of the configuration options in
configuration.pl to control the operators and connectives used to form meningful
names for invented predicates. Further information can be found in the
documentation of the configuration file itself.

The metasplain configuration is placed in the root directory of the project, on
the same level as metasplain.pl.

metasplain uses the options set in the configuration file to create meaningful
names for predicates in two steps: 

* first, head literals of invented predicates' clauses are renamed; this resuls
  in clauses _of the same predicate_ that have _different_ symbols for their
  head literals.

* second, these new predicate symbols, that are different for each clause, are
  connected together using a connective such as "or" (most likely) or "and",
  etc.

#### Clause explanations.

Meaningful names of clauses are formed by combining the symbols of body literals
in clauses of invented predicates with a set of _explanation operators_.

Explanation operators are ground unit clauses of the predicates: prefix/1,
infix/1, suffix/1.

Explanation operators are defined in the configuration option
explanation_operators/2. Following are a few examples:

```prolog
explanation_operators(identity,[prefix('')]).
explanation_operators(inverse,[prefix(anti)]).
explanation_operators(chain,[infix(of)]).
```

According to these options, names of clauses of invented predicates matching,
e.g., the "chain" metarule will be formed by infixing the symbols of their body
literals with the word "of", etc. 

For example, in the following query, the invented symbol p_1/2 is renamed to
"q_of_r/2" by connecting the symbols of its body literals, q and r by "of":

```prolog
?- _I = 1, _Ps = [(p(A,B):-p_1(A,B)), (p_1(A,B):-q(A,C),r(C,B))], invention_explanation(_I,_Ps,Es).
Es = [(p(A, B):-q_of_r(A, B)),  (q_of_r(A, B):-q(A, C), r(C, B))].
```

#### Named metarules.

Clause explanations require metarules to be _named_ (unlike in Metagol).
Metarule names for metasplain are defined in the configuration option
named_metarule/1, alongside the corresponding metarule in Metagol's notation.
For example:

```prolog
named_metarule(identity,metarule([P,Q],([P,A,B]:-[[Q,A,B]]))).
named_metarule(inverse,metarule([P,Q],([P,A,B]:-[[Q,B,A]]))).
named_metarule(chain,metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]]))).
```

Metarule names need not be meaningful themselves. They are only used as
references to connect metarules with their explanation operators. They may
therefore be any Prolog atom (a constant) including automatically assigned
numbers, etc.

#### Predicate explanations.

Meaningful names for invented predicates are created by combining the meaningful
names assigned to their clauses by means of an _explanation connective_.

The explanation connective is always the same for all invented predicates in the
program. It is a ground unit clause of the predicate connective/1, defined in
the configuration option explanation_connectives/1. For example:

```prolog
explanation_connectives([connective(or)]).
```

Given that definite clauses are disjunctions of literals the connective that
will make sense most of the time is the word "or", however, the user is free to
define any connectives such as "and", "however", "from_to" etc.

### Configuring interpretation strigns

Coming soon.

Tips and tricks
---------------

### Recursive calls.

Body literals recursively calling an invented head literal are assigned a
special symbol to signify recursion. This is defined in the configuration option
recursion_explanation/1. For example:

```prolog
% recursion_explanation(this).

?- _Ps = [(p(A,B):-p_1(A,B)), (p_1(A,B):-q(A,C), p_1(C,B))], invention_explanation(_Ps,Es).
Es = [(p(A, B):-q_of_this(A, B)),  (q_of_this(A, B):-q(A, C), q_of_this(C, B))].
```

### Left-recursive identities.

Assigning the empty string as an infix operator for the identity metarule can
result in unexpected left-recursive predicates. For example:

```prolog
% explanation_operators(identity,[prefix('')]).
% explanation_operators(inverse,[prefix(anti)]).


?- _Ps = [(p(A,B):-p_1(A,B)), (p_1(A,B):-p_2(A,B)), (p_2(A,B):-r(B,A))], invention_explanation(_Ps,Es).
Es = [(p(A, B):-anti_r(A, B)),  (anti_r(A, B):-anti_r(A, B)),  (anti_r(A, B):-r(B, A))].
```

In the example above, the invented predicate p_2/2 is first explained as
anti_r/2, then p_1/2 is explained by prefixing this new symbol with '' resulting
in a new symbol identical to anti_r/2 and turning the clause of p_1/2 into a
clause of anti_r/2 that calls itself recursively. This changes the semantics of
the program and may cause it to go into an infinite loop on execution. To avoid
this, instead assign a non-blank prefix:

```prolog
% explanation_operators(identity,[prefix('the')]).
% explanation_operators(inverse,[prefix(anti)]).

 ?-  _Ps = [(p(A,B):-p_1(A,B)), (p_1(A,B):-p_2(A,B)), (p_2(A,B):-r(B,A))], invention_explanation(_Ps,Es).
Es = [(p(A, B):-the_anti_r(A, B)),  (the_anti_r(A, B):-anti_r(A, B)),  (anti_r(A, B):-r(B, A))].
```

Further work
------------

### Mutual recursion

metasplain can't currently deal with mutually recursive invented predicates. It
will succeed in processing a program with mutual recursions between invented
predicates but will not be able to give meaningful names to all its invented
predicates. For example:

```prolog
?- _Ps = [(p(A,B):-p_1(A,B)), (p_1(A,B):-q(A,C),p_2(C,B)), (p_2(A,B):-p_1(B,A))], invention_explanation(_Ps,Es).
Es = [(p(A, B):-q_of_anti_p_1(A, B)),  (q_of_anti_p_1(A, B):-q(A, C), anti_p_1(C, B)),  (anti_p_1(A, B):-q_of_anti_p_1(B, A))].
```

### Abstraction

metasplain does not currently support abstraction. Explanations may be
formed incorrectly for interpreted BK predcates.

### Automatic interpretations

Rather than eliciting a human interpretation to replace a long automatic
explanation, it might be possible to elicit further domain knowledge from the
user to produce such interpretations automatically.

For example, the user might provide a mapping between "mother or father" and
"parent", then metasplain could automaticallly explain predicates such as
"mother_of_father" in one of the examples above with "parent".
