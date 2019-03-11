Metasplain - assign meaningful names to invented predicates.
============================================================

Motivation
----------

Predicates invented by metagol in the course of learning a new hypothesis have
symbols formed by indexing the symbol of the learning target with a number- e.g.
deep_learning_pope_1/2, platypus_engineer_24/3, etc.

A preponderance of invented predicates, especially invented predicates calling
each other, may result in a learned hypothesis that is hard to read. Replacing
invented symbols with meaningful names can help alleviate this, to some extent.

One way to determine what names it is meaningful to assign to invented
predicates is to elicit (more) domain knowledge from the user. This is the
approach taken by metasplain, where this knowledge takes the form of natural
language expressions that are then used to combine together the predicate
symbols of invented predicates' body literals. 

In the following example, the invented predicate p_1/2 is renamed to
"father_of_mother/2", by use of the expression "of" to connect the symbols of
its body literals, father/2 and mother/2:

```
% Original invented predicate
p_1(A,B) :- father(A,C), mother(C,B).

% Renamed invented predicate
father_of_mother(A,B) :- father(A,C), mother(C,B).
```

The connecting expression "of" was provided by the user, as an _infix operator_
associated with the _chain_ metarule. When a clause of p_1/2 was found matching
the chain metarule, the infix operator "of" was used to string together the
predicate symbols of p_1/2's body literals, father/2 and mother/2. 

The resulting name can be read as "the father of (someone's) mother", indicating
that the invented predicate represents this concept. 

Usage requirements
------------------

metasplain requires Swi-Prolog, v.7.0 or higher. It may also run on Yap and
other Prologs, but that has not been tested.

Examples of usage
-----------------

1) Loading metasplain.

To begin using metasplain, consult the source file load.pl, in metasplain's top
directory. This will start Swi-Prolog's documentation browser and navigate to
this README file. 

It will also open metasplain's source files in the Swi-Prolog IDE. If this is
not desired, metasplain can be started in "headless" mode by consulting
load_headless.pl instead. This will only consult the project's source files,
without starting the documentation server, or the IDE.

2) Running metasplain.

The main predicate in metasplain is invention_explanation/3. The following is
an example query:

```
?- _Ps = [(grandparent(A,B):-grandparent_1(A,C),grandparent_1(C,B)),(grandparent_1(A,B):-mother(A,B)),(grandparent_1(A,B):-father(A,B))].
true.

?- get_option(max_inv_preds(_I)), invention_explanation(_I,$_Ps,_Es), nl, writeln('Learned program:'), print_programs(_, [$_Ps]), writeln('Program with explanations:'), print_programs(_,[_Es]).

Learned program:
grandparent(A,B):-grandparent_1(A,C),grandparent_1(C,B).
grandparent_1(A,B):-mother(A,B).
grandparent_1(A,B):-father(A,B).

Program with explanations:
grandparent(A,B):-mother_or_father(A,C),mother_or_father(C,B).
mother_or_father(A,B):-mother(A,B).
mother_or_father(A,B):-father(A,B).

true.
```

invention_explanation/3 takes as input an integer, the maximum number of
invented predicates in a MIL problem, and a definite datalog program as a list
of clauses that may include invented literals. It outputs the input program
where predicate symbols of invented predicates are replaced with meaningful
names, derived from those predicates' body literals.

The predicate get_option/1 in the example query above is defined in Metagol. If
metagol.pl is not loaded, simply pass a positive integer to
invention_explanation/3:

```
?- ... invention_explanation(10,_Ps,_Es) ... 
```

In the example above, note that the order of the predicate symbols "father" and
"mother" in the meaningful name assigned to grandparent_1/2 is the same as the
order in which the clauses of grandparent_1/2 that call these predicates are
found in the learned program. This ensures that the semantics of the learned
program are not disturbed by the renaming operation.

Configuring explanation operators and connectives
-------------------------------------------------

In this section, we explain the use of the metasplain configuration file,
configuration.pl, to control the operators and connectives used to form
meningful names for invented predicates. Further information can be found in the
documentation of the configuration file itself.

The metasplain configuration isplaced in the root directory of the project, on
the same level as metasplain.pl.

metasplain uses the options set in the configuration file to create meaningful
names for predicates in two steps: 

* first, head literals of invented predicates' clauses are renamed; this resuls
  in clauses _of the same predicate_ that have _different_ symbols for their
  head literals.

* second, these new predicate symbols, that are different for each clause, are
  connected together using a connective such as "or" (most likely) or "and",
  etc.

1) Clause explanations.

Meaningful names of clauses are formed by combining the symbols of body literals
in clauses of invented predicates with a set of _explanation operators_.

Explanation operators are ground unit clauses of the predicates: prefix/1,
infix/1, suffix/1.

Explanation operators are defined in the configuration option
explanation_operators/2. Following are a few examples:

```
explanation_operators(identity,[prefix('')]).
explanation_operators(inverse,[prefix(anti)]).
explanation_operators(chain,[infix(of)]).
```

According to these options, names of clauses of invented predicates matching,
e.g., the "chain" metarule will be formed by infixing the symbols of their body
literals with the word "of", etc. 

For example, in the following query, the invented symbol p_1/2 is renamed to
"q_of_r/2" by connecting the symbols of its body literals, q and r by "of":

```
?- _I = 1, _Ps = [(p(A,B):-p_1(A,B)), (p_1(A,B):-q(A,C),r(C,B))], invention_explanation(_I,_Ps,Es).
Es = [(p(A, B):-q_of_r(A, B)),  (q_of_r(A, B):-q(A, C), r(C, B))].
```

2) Named metarules.

Clause explanations require metarules to be _named_ (unlike in Metagol).
Metarule names for metasplain are defined in the configuration option
named_metarule/1, alongside the corresponding metarule in Metagol's notation.
For example:

```
named_metarule(identity,metarule([P,Q],([P,A,B]:-[[Q,A,B]]))).
named_metarule(inverse,metarule([P,Q],([P,A,B]:-[[Q,B,A]]))).
named_metarule(chain,metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]]))).
```

Metarule names need not be meaningful themselves. They are only used as
references to connect metarules with their explanation operators. They may
therefore be any Prolog atom (a constant) including automatically assigned
numbers, etc.

3) Predicate explanations.

Meaningful names for invented predicates are created by combining the meaningful
names assigned to their clauses by means of an _explanation connective_.

The explanation connective is always the same for all invented predicates in the
program. It is a ground unit clause of the predicate connective/1, defined in
the configuration option explanation_connectives/1. For example:

```
explanation_connectives([connective(or)]).
```

Given that definite clauses are disjunctions of literals the connective that
will make sense most of the time is the word "or", however, the user is free to
define any connectives such as "and", "however", "from_to" etc.

Tips and tricks
---------------

1) Invented predicates calling invented predicates.

Care is taken in metasplain to ensure that calls to invented predicates in the
body of other invented predicates are named correctly:

```
?- _Ps=[(grandfather(A,B):-grandfather_1(A,B)),(grandfather_1(A,B):-grandfather_2(A,B)),(grandfather_1(A,B):-grandfather_3(A,B)),(grandfather_2(A,B):-father(A,C),father(C,B)),(grandfather_3(A,B):-father(A,C),mother(C,B))].
true.

?- get_option(max_inv_preds(_I)), invention_explanation(_I,$_Ps,_Es), nl,writeln('Learned program:'), print_programs(_, [$_Ps]), writeln('Program with explanations:'), print_programs(_,[_Es]).

Learned program:
grandfather(A,B):-grandfather_1(A,B).
grandfather_1(A,B):-grandfather_2(A,B).
grandfather_1(A,B):-grandfather_3(A,B).
grandfather_2(A,B):-father(A,C),father(C,B).
grandfather_3(A,B):-father(A,C),mother(C,B).

Program with explanations:
grandfather(A,B):-father_of_father_or_father_of_mother(A,B).
father_of_father_or_father_of_mother(A,B):-father_of_father(A,B).
father_of_father_or_father_of_mother(A,B):-father_of_mother(A,B).
father_of_father(A,B):-father(A,C),father(C,B).
father_of_mother(A,B):-father(A,C),mother(C,B).

true.
```

However, note that this may result in long predicate names, where there is a
long chain of calls between invented predicates, particularly in a large
program with many clauses.

For instance:

```
?- _Ps = [(great_grandparent(A,B):- great_grandparent_1(A,C), great_grandparent_2(C,B)), (great_grandparent_1(A,B):- father(A,B)), (great_grandparent_1(A,B):- mother(A,B)), (great_grandparent_2(A,B):- great_grandparent_3(A,B)), (great_grandparent_2(A,B):- great_grandparent_4(A,B)), (great_grandparent_3(A,B):- father(A,C), great_grandparent_1(C,B)), (great_grandparent_4(A,B):- mother(A,C), great_grandparent_1(C,B))].
true.

?- _I = 4, invention_explanation(_I,$_Ps,_Es), nl, writeln('Learned program:'), print_programs(_, [$_Ps]), writeln('Program with explanations:'), print_programs(_,[_Es]).

Learned program:
great_grandparent(A,B):-great_grandparent_1(A,C),great_grandparent_2(C,B).
great_grandparent_1(A,B):-father(A,B).
great_grandparent_1(A,B):-mother(A,B).
great_grandparent_2(A,B):-great_grandparent_3(A,B).
great_grandparent_2(A,B):-great_grandparent_4(A,B).
great_grandparent_3(A,B):-father(A,C),great_grandparent_1(C,B).
great_grandparent_4(A,B):-mother(A,C),great_grandparent_1(C,B).

Program with explanations:
great_grandparent(A,B):-father_or_mother(A,C),father_of_father_or_mother_or_mother_of_father_or_mother(C,B).
father_or_mother(A,B):-father(A,B).
father_or_mother(A,B):-mother(A,B).
father_of_father_or_mother_or_mother_of_father_or_mother(A,B):-father_of_father_or_mother(A,B).
father_of_father_or_mother_or_mother_of_father_or_mother(A,B):-mother_of_father_or_mother(A,B).
father_of_father_or_mother(A,B):-father(A,C),father_or_mother(C,B).
mother_of_father_or_mother(A,B):-mother(A,C),father_or_mother(C,B).
```

Clearly, "father_of_father_or_mother_or_mother_of_father_or_mother" is only
marginally easier to understand, if at all, than "great_grandparent_2".


2) Recursive calls.

Body literals recursively calling an invented head literal are assigned a
special symbol to signify recursion. This is defined in the configuration option
recursion_explanation/1. For example:

```
recursion_explanation(this).
```

This would result in the following:

```
?- _I = 1, _Ps = [(p(A,B):-p_1(A,B)), (p_1(A,B):-q(A,C), p_1(C,B))], invention_explanation(_I,_Ps,Es).
Es = [(p(A, B):-q_of_this(A, B)),  (q_of_this(A, B):-q(A, C), q_of_this(C, B))].
```

3) Redundant identities.

Assigning the empty string as an infix operator for the identity metarule can
result in a redundant predicate. For example:

```
% explanation_operators(identity,[prefix('')]).

?- _I = 2, _Ps = [(p(A,B):-p_1(A,B)), (p_1(A,B):-p_2(A,B)), (p_2(A,B):-r(B,A))], invention_explanation(_I,_Ps,Es).
Es = [(p(A, B):-anti_r(A, B)),  (anti_r(A, B):-anti_r(A, B)),  (anti_r(A, B):-r(B, A))].
```

This is not necessarily undesirable. Note that in the example above the
predicate p_1/2 is redundant- it is only re-defining p_2/2 with a different
invented symbol. Sorting the program once p_1/2 is assigned the name anti_r/2
will remove the duplicate clause.

4) Incorrect maximum number of invented predicates.

If an overly low number is given as the maximum number of invented predicates,
some of the invented predicates in the program will not be renamed:

```
?- _I = 1, _Ps = [(p(A,B):-p_1(A,B)), (p_1(A,B):-q(A,C),p_2(C,B)), (p_2(A,B):-r(B,A))], invention_explanation(_I,_Ps,Es).
Es = [(p(A, B):-q_of_p_2(A, B)),  (q_of_p_2(A, B):-q(A, C), p_2(C, B)),  (p_2(A, B):-r(B, A))].
```

Above, the predicate p_2/2 is not renamed because the maximum number of invented
predicates is incorrectly given as 1. Accordingly, the invented predicate p_1/2
calling p_2/2 is renmaed to q_of_p_2/2.

Again, this may actually be desired by the user, perhaps in order to examine
some property of specific invented predicates. Note however that metasplain will
name exactly I predicates, where I the maximum provided, and it will do so in
the order in which these predicates are encountered. There is no way to specify
exactly which invented predicate will remain unnamed.

Further work
------------

1) Mutual recursion

metasplain can't currently deal with mutually recursive invented predicates. It
will succeed in processing a program with mutual recursions between invented
predicates but will not be able to give meaningful names to all its invented
predicates. For example:

```
_I = 2, _Ps = [(p(A,B):-p_1(A,B)), (p_1(A,B):-q(A,C),p_2(C,B)), (p_2(A,B):-p_1(B,A))], invention_explanation(_I,_Ps,Es).
Es = [(p(A, B):-q_of_anti_p_1(A, B)),  (q_of_anti_p_1(A, B):-q(A, C), anti_p_1(C, B)),  (anti_p_1(A, B):-q_of_anti_p_1(B, A))].
```

2) Abstraction

metasplain does not currently support abstraction. Explanations may be
formed incorrectly for interpreted BK predcates.


3) Shorter names

As a program grows, and the number of invented predicates grows, meaningful
names may become large and unwieldy. See for instance the example in the section
"Invented predicates calling invented predicates", where a invented predicate is
assigned the name *father_of_father_or_father_of_mother*.

It is possible to shorten such long names by allowing the user to provide a
_substitute symbol_ for them. In the first example in the _Tips and Trokcs_
section above, a user could provide a substitute symbol "parent", then
metasplain could automatically replace all occurrences of
*father_of_father_or_father_of_mother* with "parent":

```
grandfather(A,B):-parent(A,B).
parent(A,B):-father_of_father(A,B).
parent(A,B):-father_of_mother(A,B).
father_of_father(A,B):-father(A,C),father(C,B).
father_of_mother(A,B):-father(A,C),mother(C,B).
```
