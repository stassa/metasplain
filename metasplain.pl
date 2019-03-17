:-module(metasplain, [invention_explanation/2
		     ,literals_symbols/2
		     ,clause_metarule/3
		     ,select_definition/4
		     ,clause_predicate_symbol/2
		     ,clause_literals/2
		     ,print_programs/2
		     ]).

:-use_module(configuration).
:-use_module(interactive).

/** <module> Give meaningful names to invented predicates.
*/

%!	invention_explanation(+Program,-Explained) is det.
%
%	Assign meaningful names to invented predicates in a Program.
%
%	invented_explanation expects a Program in the form of a list of
%	clauses output by Metagol with any number of invented predicates
%	between 0 and Invented.
%
%	Because of the order in which Metagol adds clauses to a
%	hypothesis, in the course of meta-interpretation, calls to (i.e.
%	body literals with the symbol of) an invented predicate should
%	be encountered in a single clause before the definition of that
%	predicate, and then only in clauses following the definition.
%
%	invention_explanation/2 exploits this to ensure that calls to
%	invented predicates are correctly renamed according to that
%	predicate's explanation, by recursively traversing the program
%	from the top-down, so that the definition of an invented
%	predicate is renamed before a predicate calling it needs to be
%	explained.
%
%	As to recursive calls in the body of invented predicates, these
%	are explained using the special renaming term defined in the
%	configuration option recursion_explanation/1.
%
%	@tbd Currently can't deal with mutual recursion.
%
invention_explanation(Ps,Es):-
	top_goal(Ps,G/_)
	,clause_explanations(Ps,G,Cs_E)
	,program_explanation(Ps,Cs_E,Es).


%!	invented_symbol(+Symbol,+Goal) is det.
%
%	True when Symbol is an invented symbol.
%
%	Goal should be the top-goal of a hypothesis including one of
%	more clauses of Symbol.
%
%	What is considered an invented symbol depends on the value of
%	the configuration option invention_assumption/1. In short,
%	either every Prolog atom followed by a numerical index of
%	integers separated by underscores is considered to be an
%	invented symbol (the "weak assumption") or only the symbol of
%	Goal followed by a numeric index, is (the "strong assumption").
%
invented_symbol(S,G):-
	configuration:invention_assumption(A)
	,atom_chars(S,Ss)
	,atom_chars(G,Gs)
	,invented_symbol(A,Ss,Gs).


%!	invented_symbol(+Strength,+Symbol,+Goal) is det.
%
%	Business end of invened_symbol/2.
%
%	Clauses are selected depending on Strength, taken from the
%	configuration option invention_assumption/1.
%
invented_symbol(weak,Ss,Gs):-
	Ss \= Gs
	,append(Gs,Rs,Ss)
	,once(phrase(index,Rs))
	,!.
invented_symbol(strong,Ss,_Gs):-
	once(phrase(indexed_symbol,Ss)).


%!	indexed_symbol is nondet.
%
%	A predicate symbol with a numeric index.
%
indexed_symbol --> symbol, index.

symbol --> [A], { atom(A) }.
symbol --> [A], { atom(A) }, symbol.

index --> underscore,number.
index --> underscore,number,index.

underscore --> ['_'].
number --> [N], {atom_chars(N,[C]), char_type(C,digit)}.


%!	clause_explanations(+Program,+Goal,-Explanations) is det.
%
%	Form Explanations for Invented predicates in a Program.
%
%	Goal is the top-goal in the Program.
%
clause_explanations(Ps,G,Es):-
	clause_explanations(Ps,G,[],Es).

%!	clause_explanations(+Program,+Goal,+Acc,-Explanations) is
%!	det.
%
%	Business end of clause_explanations/3.
%
%	Explanations is a list of key-value pairs, S-E output by
%	clause_explanation/3, where each key, S, is the symbol of an
%	invented predicate and each value, E, is an explanation of one
%	of the clauses of that predicate in the Program.
%
clause_explanations([],_G,Es,Es):-
	!.
clause_explanations([C|Ps],G,Acc,Bind):-
	top_goal([C],S/_A)
	,predicate_explanation(S,[C|Ps],G,Acc,Acc_,_E,_Ps)
	,!
	,clause_explanations(Ps,G,Acc_,Bind).
clause_explanations([_|Ps],G,Acc,Bind):-
	clause_explanations(Ps,G,Acc,Bind).


%!	explained_clause(+Clause,+Goal,+Known,-Explained) is det.
%
%	Explain a Clause of an invented predicate.
%
%	Goal is the top goal of the Clause's parent program, used to
%	identify invented predicate symbols. If the symbol of the head
%	literal of Clause is invented, Explained is a key-value pair
%	S-E, where S that symbol and S an explanation of the invented
%	predicate formed by combining the symbols of the body literals
%	in Clause with the explanation operators assigned in the
%	configuration to the first metarule matched by the clause.
%
%	Known is a list of _predicate_ explanations formed so far. It is
%	a list of key-value pairs, in particular, it's the
%	accumulator variable in clause_explanations/4, which means that
%	it's a list of key-value pairs, S-E, where each key, S, is
%	the symbol of an invented predicate and each value, E, is
%	a predicate explanation formed for this predicate. Where a body
%	literal of Clause is encountered during processing that has S as
%	its symbol, that symbol is replaced with E in the clause
%	explanation of Clause output in Explained.
%
explained_clause(C,Ps,G,Ks,Ks_,S-E):-
	clause_literals(C,Ls)
	,literals_symbols(Ls, [S|Ss])
	,invented_symbol(S,G)
	,clause_operators(C,[S|Ss],Os)
	,clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_,Es)
	,atomic_list_concat(Es,'_',E).


%!	clause_explanation(+Sym,+Syms,+Ops,+Prog,+Goal,+Known,-New,-String)
%!	is det.
%
%	Form an explanation String for an invented predicate clause.
%
clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_,Es):-
	once(clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_,[],Es)).

%!	clause_explanation(+Sym,+Syms,+Ops,+Prog,+Goal,+Known,-New,+Acc,-String)
%!	is det.
%
%	Business end of clause_explanation/8.
%
%	Essentially a parser for explanation operators, outputting
%	clause explanation strings. Unfortunately, in order to form a
%	clause explanation we may have to form an explanation for one of
%	the clause's literals, so this calls predicate_explanation/7
%	which eventually calls this one recursively. Bottom line, this
%	gets very complicated to follow.
%
%	Sym is the predicate symbol of the current clause for which we
%	want to form an explanation. It is used to avoid forming an
%	explanation for a different clause and offering it up as an
%	explanation for one of this predicate's clauses.
%
%	Syms are the predicate symbols of the current clause's body
%	literals.
%
%	Ops is the list of explanation operators assigned to
%	the metarule matching this clause. Prog is the current program,
%	that should at this point start with the clause we want to form
%	an explanation for. Goal is the top goal of the clause's parent
%	program used to determine which litearls in the clause's body to
%	explain (we don't want to explain non-invented literals!).
%
%	Known is the accumulated list of _predicate_ explanations
%	so-far, a list of Symbol-Explanation pairs. It is used to allow
%	predicates calling a predicate defined preceding to their own to
%	use the explanation of that predicate formed earlier.
%
%	New is the list Known augmented with any new predicate
%	explanations derived during the explanation of the current
%	clause, i.e. the predicates whose atoms are body literals in the
%	clause.
%
%	Acc is the accumulator of the current explanation's symbols, and
%	String is the finished job.
%
%	@tbd: The explanation string is reversed at the end of
%	processing. This is because we allow multiple operators to apply
%	to a string of more than two symbols. We process the lists of
%	operators and their corresponding symbols using a recursive
%	list-traversal skeleton that means the result is in reverse
%	order at the end, so we must reverse it again. This in turn
%	requires operands to be combined with operators _in reverse
%	order_, i.e. we reverse the operands during the application of
%	the operator.
%
clause_explanation(_S,[],[],_Ps,_G,Ks,Ks,Es_,Es):-
	reverse(Es_,Es)
	,!.

% A body literal has an invented symbol and might need explanation.
clause_explanation(S,[Si|Ss],Os,Ps,G,Ks,Ks_Bind,Acc,Bind):-
	invented_symbol(Si,G)
	,start_of_definition(Si,Ps,Ds)
	,predicate_explanation(Si,Ds,G,Ks,Ks_,Si-E,Ps_)
	,clause_explanation(S,[E|Ss],Os,Ps_,G,Ks_,Ks_Bind,Acc,Bind).
clause_explanation(S,[S1,S2|Ss],Os,Ps,G,Ks,Ks_Bind,Acc,Bind):-
	invented_symbol(S1,G)
	,start_of_definition(S1,Ps,Ds)
	,predicate_explanation(S1,Ds,G,Ks,Ks_,S1-E,Ps_)
	,clause_explanation(S,[E,S2|Ss],Os,Ps_,G,Ks_,Ks_Bind,Acc,Bind).
clause_explanation(S,[S1,S2|Ss],Os,Ps,G,Ks,Ks_Bind,Acc,Bind):-
	invented_symbol(S2,G)
	,start_of_definition(S2,Ps,Ds)
	,predicate_explanation(S2,Ds,G,Ks,Ks_,S2-E,Ps_)
	,clause_explanation(S,[S1,E|Ss],Os,Ps_,G,Ks_,Ks_Bind,Acc,Bind).

% Explanation operators with blank expressions.
clause_explanation(S,[Si|Ss],[O|Os],Ps,G,Ks,Ks_Bind,Acc,Bind):-
	O =.. [_|['']]
	,clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_Bind,[Si|Acc],Bind).
clause_explanation(S,[S1,S2|Ss],[infix('')|Os],Ps,G,Ks,Ks_Bind,Acc,Bind):-
	clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_Bind,[S2,S1|Acc],Bind).

% Explanation operators dealing with recursion.
clause_explanation(S,[S|Ss],[prefix(E)|Os],Ps,G,Ks,Ks_Bind,Acc,Bind):-
	configuration:recursion_explanation(R)
	,clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_Bind,[R,E|Acc],Bind).
clause_explanation(S,[S,S2|Ss],[infix(E)|Os],Ps,G,Ks,Ks_Bind,Acc,Bind):-
	configuration:recursion_explanation(R)
	,clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_Bind,[S2,E,R|Acc],Bind).
clause_explanation(S,[S1,S|Ss],[infix(E)|Os],Ps,G,Ks,Ks_Bind,Acc,Bind):-
	configuration:recursion_explanation(R)
	,clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_Bind,[R,E,S1|Acc],Bind).
clause_explanation(S,[S|Ss],[suffix(E)|Os],Ps,G,Ks,Ks_Bind,Acc,Bind):-
	configuration:recursion_explanation(R)
	,clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_Bind,[E,R|Acc],Bind).

% Nice, uncomplicated explanation operators.
clause_explanation(S,[Si|Ss],[prefix(E)|Os],Ps,G,Ks,Ks_Bind,Acc,Bind):-
	clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_Bind,[Si,E|Acc],Bind).
clause_explanation(S,[S1,S2|Ss],[infix(E)|Os],Ps,G,Ks,Ks_Bind,Acc,Bind):-
	clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_Bind,[S2,E,S1|Acc],Bind).
clause_explanation(S,[Si|Ss],[suffix(E)|Os],Ps,G,Ks,Ks_Bind,Acc,Bind):-
	clause_explanation(S,Ss,Os,Ps,G,Ks,Ks_Bind,[E,Si|Acc],Bind).


%!	start_of_definition(+Predicate,+Program,-Definition) is det.
%
%	Find the start of a Predicate's definition in a Progam.
%
start_of_definition(S,Ps,Ds):-
	start_of_definition_(S,Ps,Ds)
	,!.
start_of_definition(_S,Ps,Ps).
% No definition in the rest of the program!
% At this point, S must have an explanation.

%!	start_of_definition_(+Predicate,+Program,-Definition) is det.
%
%	Business end of start_of_definition/3.
%
start_of_definition_(S,[C|Ps],[C|Ps]):-
	top_goal([C],S/_A).
start_of_definition_(S,[_C|Ps],Bind):-
	start_of_definition_(S,Ps,Bind).


%!	literals_symbols(+Literals,-Symbols) is det.
%
%	Collect the predicate symbols in a list of Literals.
%
%	@tbd This has an unusual (for me anyway) reverse/2 call in the
%	_interface_ clause. Normally, I reverse the result in the
%	auxiliary, as soon as I've collected a list that needs
%	reversing. However, in literals_symbols/3, i.e. this one's
%	auxiliary, there is a double-dip recursion skeleton which
%	means that, if the boundary condition reverses the result,
%	the result will be reversed twice (once in the first and again
%	in the second recursive call). The simplest thing to do is to
%	collect the result, without disturbing its order, and reverse it
%	afterwards, i.e. on exit from literals_symbols/3, in this
%	predicate. Interesting, innit?
%
literals_symbols(Ls,Ss):-
	literals_symbols(Ls,[],Ss_)
	,reverse(Ss_,Ss).

%!	literals_symbols(+Literals,+Acc,-Symbols) is det.
%
%	Business end of literals_symbols/2.
%
%	See notes in parent about the lack of reverse/2 call here in
%	relation to the double-recursion in the second clause. The
%	reason for the double recursion is that this predicate must be
%	able to collect symbols of second-order literals, which are,
%	themselves, first-order atoms ... or possibly second order ones,
%	even.
%
literals_symbols([],Ss,Ss):-
	!.
literals_symbols([L|Ls],Acc,Bind):-
	second_order(L)
	,!
	,L =.. [S|As]
	,literals_symbols(As,[S|Acc],Acc_)
	,literals_symbols(Ls,Acc_,Bind).
literals_symbols([L|Ls],Acc,Bind):-
% L may be a term in a second-order literal.
	compound(L)
	,!
	,functor(L,S,_A)
	,literals_symbols(Ls,[S|Acc],Bind).
literals_symbols([_L|Ls],Acc,Bind):-
	literals_symbols(Ls,Acc,Bind).


%!	second_order(+Literal) is det.
%
%	True when Literal is a second-order atom.
%
second_order(L):-
	compound(L)
	,L =.. Ls
	,member(Li,Ls)
	,compound(Li).


%!	clause_operators(+Clause,+Symbols,-Operators) is det.
%
%	Collect explanation Operators for a Clause.
%
clause_operators(C,Ss,Os):-
	once(clause_metarule(C,Ss,M))
	,configuration:explanation_operators(M,Os).


%!	clause_metarule(+Clause,+Symbols,-Metarule) is det.
%
%	Find the name of a Metarule matched by a Clause.
%
%	@tbd This is a little on the redundant side. We could just add
%	both calls to clause_operators/3 without growing it that much in
%	size.
%
clause_metarule(C,Ss,M):-
	named_metarule(M,metarule(Ss,Ls))
	,metarule_clause(metarule(Ss,Ls),C).


%!	metarule_clause(+Metarule,+Clause) is det.
%
%	True when Clause matches Metarule.
%
%	A Clause matches a Metarule when their literals match. To
%	determine this, we instantiate the literals of the metarule and
%	attempt to unify the result with Clause. The second-order terms
%	in Metarule are already bound to the predicate symbols of
%	literals in Clause, in clause_operators/3.
%
metarule_clause(M,(H:-Bs)):-
	compound(H)
	,!
	,metarule_literals(M,Ls_)
	,copy_term((H:-Bs),(H_:-Bs_))
	,numbervars((H_:-Bs_))
	,once(list_tree(Ls_,(H_,Bs_))).
metarule_clause(M,(H:-Bs)):-
% Used by interpretation:interpretation_program/3.
	var(H)
	,var(Bs)
	,metarule_literals(M,Ls_)
	,once(list_tree(Ls_,(H,Bs))).


%!	metarule_literals(+Metarule,-Literals) is det.
%
%	Extract literals from a Metarule declaration.
%
%	Copied from Metasmith's theory_generator.pl
%
%	Metarule is a partially ground clause of a metarule, with
%	higher-order, existentially quantified variables insantiated.
%
%	Note that Literals is a list of actual literals, not lists of
%	terms, as in the Metarule declaration used in Metagol. In other
%	words, the lists of terms in Metarule are converted to
%	predicates' atoms (compound terms in Prolog).
%
metarule_literals(metarule(Es,Ls),Ls_):-
	metarule_literals(Ls,Es,[],Ls_).

%!	metarule_literals(+Metarule,+Existential,+Acc,-Literals) is det.
%
%	Business end of metarule_literals/2.
%
metarule_literals([],_Es,Acc,Ls):-
	!
	,reverse(Acc,Ls).
metarule_literals((H:-Bs),Es,Acc,Bind):-
% H is a list of terms, Bs is a list of lists of terms
	!
	,H_ =.. H
	,metarule_literals(Bs,Es,[H_|Acc],Bind).
metarule_literals([L|Ls],Es,Acc,Bind):-
	!
	,instantiated_literal(L,Es,L_)
	,metarule_literals(Ls,Es,[L_|Acc],Bind).


%!	instantiated_literal(+Literal,+Existential,-Instantiated) is
%!	det.
%
%	Instantiate a Literal of a metarule.
%
%	An "instantiated literal" is an atom constructed from a list of
%	terms representing a literal in Metagol's notation.
%
%	Literal is such a list of terms for the metarule currently
%	being processed (by metarule_clause/2 and metarule_literals/3).
%	Existential is the list of existentially quantified terms in
%	that metarule. Instantiated is the atom constructed from the
%	list of terms.
%
%	This predicate ensures that second-order literals are correctly
%	instantiated. A second-order literal is represented as a list
%	[S,T1,...,Tn] where S is a second-order predicate and at least
%	one of its terms Ti is itself a list representing an atom of
%	a predicate passed as an argument to S.
%
%	However, it is always possible that S also takes an ordinary
%	list as an argument, i.e one that does _not_ represent an atom
%	of a predicate. Unfortunately, it's impossible to syntactically
%	distinguish the two kinds of list. Instead, we scan the list
%	Existential, of the existentially quantified terms in the
%	metarule, for each member of Literal that is a list. If a member
%	of Literal is a list and is _not_ in Existential then it's a
%	list representing an atom of a predicate that is an argument to
%	S. Otherwise, the list is just a first-order term in S.
%
instantiated_literal(L,Es,L_):-
	instantiated_literal(L,Es,[],L_).

instantiated_literal([],_Es,Acc,L):-
	reverse(Acc,Ts)
	,L =.. Ts
	,!.
instantiated_literal([T|Ts],Es,Acc,Bind):-
	is_list(T)
	,\+ member(T, Es)
	,!
	,T_ =.. T
	,instantiated_literal(Ts,Es,[T_|Acc],Bind).
instantiated_literal([T|Ts],Es,Acc,Bind):-
	instantiated_literal(Ts,Es,[T|Acc],Bind).


%!	predicate_explanation(+Sym,+Program,+Goal,+Known,-New,-Explanation,-Rest)
%!	is det.
%
%	Form an explanation for a predicate with an invented Symbol.
%
%	Sym is the symbol of the predicate we wish to form an
%	explanation for, given so that we don't end up outputting an
%	explanation for the wrong predicate.
%
%	Clauses of that predicate found in Program are explained
%	and their explanations combined with the current explanation
%	connective, to form an explanation of the predicate as a whole.
%	Clauses remaining unexplained in Program, that should all have a
%	predicate symbol other than Symbol, are added to Rest, for
%	further processing.
%
%	If the configuration option interactive_session/1 is set to
%	"true", this will call user_explanation/4 to begin an
%	interactive user session in order to elicit a human
%	interpretation of the automatically formed Explanation. This
%	interpretation will then replace the automatic Explanation.
%	If interactive_session/1 is set to "false", no interaction with
%	the user will be initiated and the automatically derived
%	Explanation will be used instead.
%
%	Program is a list of clauses and Goal is the top-goal of
%	Program, used to identify invented symbols. Explanations is a
%	key-value pair, S-E, where S the input Symbol and E is the
%	program explanation of the predicate.
%
%	Known and New are the list of predicate explanations obtained
%	so-far, and the same list augmented with the explanation of the
%	current predicate.
%
predicate_explanation(S,Ps,G,Ks,Ks_,S-E,Rs):-
	interactive_session(false)
	,!
	,predicate_explanation_(S,Ps,G,Ks,Ks_,S-E,Rs).
predicate_explanation(S,Ps,G,Ks,Ks2,S-UE,Rs):-
	interactive_session(true)
	,predicate_explanation_(S,Ps,G,Ks,Ks1,S-E,Rs)
	,user_explanation(S-E,Ks1,S-UE,Ks2).


%!	predicate_explanation_(+Sym,+Program,+Goal,+Known,-New,-Explanation,-Rest)
%!	is det.
%
%	Business end of predicate_explanation/7.
%
%	Split off to allow predicate_explanation/7 to start an
%	interactive user session if required.
%
predicate_explanation_(S,Ps,_G,Ks,Ks,S-E,Ps):-
	memberchk(S-E,Ks)
	,!.
predicate_explanation_(S,Ps,G,Ks,Ks_,PE,Rs):-
	predicate_explanation(S,Ps,G,Ks,Ks_,[],PE,Rs).

%!	predicate_explanation(+Sym,+Prog,+Goal,+Known,-New,+Acc,-Explanations,-Rest)
%!	is det.
%
%	Business end of predicate_explanation/6.
%
%	Acc is the accumulator for clause explanations that are used to
%	form the predicate explanation for the predicate with the given
%	Sym(bol).
%
predicate_explanation(S,[C|Ps],G,Ks,Ks_Bind,Acc_Es,Bind_Es,Bind_Rs):-
	explained_clause(C,Ps,G,Ks,Ks_,S-E)
	,!
	,predicate_explanation(S,Ps,G,Ks_,Ks_Bind,[E|Acc_Es],Bind_Es,Bind_Rs).
predicate_explanation(S,Ps,_G,Ks,[S-E|Ks],Acc_Es,S-E,Ps):-
	configuration:explanation_connectives([Ns])
	,reverse(Acc_Es,Acc_Es_)
	,once(phrase(predicate_explanation(Ns,Es),Acc_Es_))
	,atomic_list_concat(Es,'_',E).


%!	predicate_explanation(+Connective,+String) is det.
%
%	Form an explanation String for an invented predicate.
%
predicate_explanation(connective(_C),[L1]) --> [L1].
predicate_explanation(connective(C), [L1,C|Ss]) -->
	[L1]
	,predicate_explanation(connective(C),Ss).


%!	program_explanation(+Program,+Predicate_Explanations,-Explained)
%!	is det.
%
%	Replace symbols of invented literals with their explanations.
%
program_explanation(Ps,Es,Ps_):-
	program_explanations(Ps,Es,[],Ps_).

%!	program_explanations(+Program,+Predicate_Explanations,+Acc,-Explained)
%!	is det.
%
%	Business end of program_explanation/3.
%
%	Replaces invented symbols of head and body literals in a Program
%	with Predicate_Explanations output by predicate_explanations/3.
%
program_explanations([],_Es,Acc,Ps):-
	reverse(Acc,Ps)
	,!.
program_explanations([C|Ps],Es,Acc,Bind):-
	explained_literals(C,Es,C_)
	,program_explanations(Ps,Es,[C_|Acc],Bind).


%!	explained_literals(+Clause,+Predicate_Explanations,-Explained)
%!	is det.
%
%	Replace invented symbols in a Clause with their explanations.
%
explained_literals(C,Es,(H:-B)):-
	clause_literals(C,Ls)
	,explained_literals(Ls,Es,[],Ls_)
	,once(list_tree(Ls_,(H,B))).

%!	explained_literals(+Clause,+Predicate_Explanations,+Acc,-Explained)
%!	is det.
%
%	Business end of explained_literals/3.
%
%	The second clause of this predicate follows a similar pattern to
%	the second clause in literals_symbols/3 when it comes to second-
%	order literals: these are recursively explained, if they have
%	invented symbols in Explanations.
%
explained_literals([],_Es,Acc,Ls):-
	reverse(Acc,Ls)
	,!.
explained_literals([L|Ls],Es,Acc,Bind):-
	second_order(L)
	,!
	,L =.. [S|As]
	,explained_literals([S],Es,[],[S_])
	,explained_literals(As,Es,[],As_)
	,L_ =.. [S_|As_]
	,explained_literals(Ls,Es,[L_|Acc],Bind).
explained_literals([L|Ls],Es,Acc,Bind):-
	nonvar(L)
	,L =.. [S|As]
	,memberchk(S-E,Es)
	,!
	,L_ =.. [E|As]
	,explained_literals(Ls,Es,[L_|Acc],Bind).
explained_literals([L|Ls],Es,Acc,Bind):-
	explained_literals(Ls,Es,[L|Acc],Bind).



%!	top_goal(+Program,-Goal) is det.
%
%	True when Goal is the top-level goal in Program.
%
%	Use this to extract the target concept from a learned
%	hypothesis, when this is not otherwise easy to obtain (e.g. from
%	a top-level query).
%
%	Goal is a predicate indicator, F/A.
%
top_goal(Hs,F/A):-
	Hs = [H:-_|_]
	,functor(H,F,A).


%!	select_definition(+Predicate,+Program,-Definition,-Rest)
%
%	Select the clauses of a Predicate Definition in a Program.
%
%	Rest are the clauses remaining Program after all clauses of
%	Predicate are removed from it.
%
select_definition(C,Ps,Ds,Rs):-
	clause_predicate_symbol(C,S/A)
	,select_definition(S/A,Ps,[],Rs,[],Ds).

select_definition(_S/_A,[],Acc_Ps,Ps,Acc_Ds,Ds):-
	reverse(Acc_Ps,Ps)
	,reverse(Acc_Ds,Ds)
	,!.
select_definition(S/A,[(H:-B)|Ps],Acc_Ps,Bind_Ps,Acc_Ds,Bind_Ds):-
	functor(H,S,A)
	,!
	,select_definition(S/A,Ps,Acc_Ps,Bind_Ps,[(H:-B)|Acc_Ds],Bind_Ds).
select_definition(S/A,[C|Ps],Acc_Ps,Bind_Ps,Acc_Ds,Bind_Ds):-
	select_definition(S/A,Ps,[C|Acc_Ps],Bind_Ps,Acc_Ds,Bind_Ds).


%!	definition(+Predicate,+Program,-Definition) is det.
%
%	Collect the clauses of a Predicate Definition in a Program.
%
%	Predicate is either a predicate indicator, Symbol/Arity, or a
%	clause of the target predicate. Definition is the list of all
%	clauses of Predicate in the list of clauses Program.
%
definition(S/A,Ps,Ds):-
	!
	,definition(S/A,Ps,[],Ds).
definition(C,Ps,Ds):-
	clause_predicate_symbol(C,S/A)
	,definition(S/A,Ps,[],Ds).

%!	definition(+Predicate,+Program,+Acc,-Definition) is det.
%
%	Business end of definition/3.
%
definition(_S/_A,[],Acc,Ds):-
	reverse(Acc,Ds).
definition(S/A,[(H:-B)|Ps],Acc,Bind):-
	functor(H,S,A)
	,!
	,definition(S/A,Ps,[(H:-B)|Acc],Bind).
definition(S/A,[_|Ps],Acc,Bind):-
	definition(S/A,Ps,Acc,Bind).


%!	clause_predicate_symbol(+Clause,-Symbol) is det.
%
%	Symbol is the predicate symbol of the head literal in Clause.
%
clause_predicate_symbol((H:-_B),F/A):-
	!
	,functor(H,F,A).
clause_predicate_symbol(L,F/A):-
	compound(L)
	,functor(L,F,A).


%!	clause_literals(+Clause, -Literals) is det.
%
%	Convert a Clause to a list of Literals.
%
clause_literals('()', []):-
	!.
clause_literals(Ts, Ls):-
	list_tree(Ls, Ts)
	,!.


%!	list_tree(?List, ?Tree) is det.
%
%	Convert between a list and a Prolog binary tree.
%
%	Tree is a Prolog term (L1,...,Ln). A binary tree, yes?
%
list_tree(Ls, Ts):-
	phrase(list_tree(Ts), Ls).


%!	list_tree(?Tree) is nondet.
%
%	Business end of clause_literals/2.
%
list_tree((T,Ts)) --> [T], list_tree(Ts).
list_tree((T:-Ts)) --> [T], list_tree(Ts).
list_tree(T) --> [T].


%!	print_programs(+Stream,+Programs) is det.
%
%	Pretty-print a list of Programs to Stream.
%
print_programs(_,Ps):-
	var(Ps)
	,!.
print_programs(S,[]):-
	!
	,(   var(S)
	 ->  S = user_output
	 ;   true
	 )
	,writeln(S,'[]').
print_programs(S,Ps):-
	(   var(S)
	->  S = user_output
	;   true
	)
	,forall(member(P,Ps)
	       ,(print_program_clauses(S,P)
		,nl(S)
		)
	       ).


%!	print_program_clauses(+Stream,+Program) is det.
%
%	Pretty-print each of a Progam's clauses to Stream.
%
print_program_clauses(_,P):-
	var(P)
	,!.
print_program_clauses(S,[]):-
	!
	,writeln(S,'[]').
print_program_clauses(S,P):-
	forall(member(C,P)
	      ,(copy_term(C, C_)
	       ,numbervars(C_,0,_)
	       ,write_term(S, C_, [fullstop(true)
				  ,nl(true)
				  ,numbervars(true)
				 ]
			  )
	       )
	      ).
