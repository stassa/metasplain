:-module(metasplain, [invention_explanation/3
		     ,print_programs/2
		     ]).

:-use_module(configuration).
:-use_module(interactive).

/** <module> Give meaningful names to invented predicates.
*/

%!	invention_explanation(+Invented,+Program,-Explained) is det.
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
%	invention_explanation/3 exploits this to ensure that calls to
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
invention_explanation(I,Ps,Es):-
	invented_symbols(I,Ps,Is)
	,clause_explanations(Ps,Is,Cs_E)
	,program_explanation(Ps,Cs_E,Es).


%!	invented_symbols(+Max,+Program,-Invented) is det.
%
%	Construct all possible Invented symbols.
%
%	Max is the maximum number of invented symbols in a hypothesis;
%	it can be obtained from the metagol option max_inv_preds/1.
%
%	Program is a learned hypothesis, that may include up to Max
%	invented predicates, from which the symbol of the learning
%	target is extracted, and used to identify invented symbols. In
%	Metagol, invented symbols are of the form Symbol_I where Symbol
%	is the symbol of the learning target and I is an index, from 1
%	to Max.
%
%	Invented is a list of all invented symbols that may be included
%	in the given Program.
%
invented_symbols(I,Ps,Ss):-
	top_goal(Ps,F/_A)
	,findall(S
	       ,(between(1,I,K)
		,atomic_list_concat([F,K],'_',S)
		)
	       ,Ss).


%!	clause_explanations(+Program,+Invented,-Explanations) is det.
%
%	Form Explanations for Invented predicates in a Program.
%
clause_explanations(Ps,Is,Es):-
	clause_explanations(Ps,Is,[],Es).

%!	clause_explanations(+Program,+Invented,+Acc,-Explanations) is
%!	det.
%
%	Business end of clause_explanations/3.
%
%	Explanations is a list of key-value pairs, S-E output by
%	clause_explanation/3, where each key, S, is the symbol of an
%	invented predicate and each value, E, is an explanation of one
%	of the clauses of that predicate in the Program.
%
clause_explanations([],_Is,Es,Es):-
	!.
clause_explanations([C|Ps],Is,Acc,Bind):-
	top_goal([C],S/_A)
	,predicate_explanation(S,[C|Ps],Is,Acc,Acc_,_E,_Ps)
	,!
	,clause_explanations(Ps,Is,Acc_,Bind).
clause_explanations([_|Ps],Is,Acc,Bind):-
	clause_explanations(Ps,Is,Acc,Bind).


%!	explained_clause(+Clause,+Invented,+Known,-Explained) is det.
%
%	Explain a Clause of an invented predicate.
%
%	Invented is a list of invented predicate symbols. If the symbol
%	of the head literal of Clause is in Invented, Explained is a
%	key-value pair S-E, where S that symbol and S an explanation of
%	the invented predicate formed by combining the symbols of the
%	body literals in Clause with the explanation operators assigned
%	in the configuration to the first metarule matched by the
%	clause.
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
explained_clause(C,Ps,Is,Ks,Ks_,S-E):-
	clause_literals(C,Ls)
	,literals_symbols(Ls, [S|Ss])
	,memberchk(S,Is)
	,clause_operators(C,[S|Ss],Os)
	,clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_,Es)
	,atomic_list_concat(Es,'_',E).


%!	clause_explanation(+Sym,+Syms,+Ops,+Prog,+Inv,+Known,-New,-String)
%!	is det.
%
%	Form an explanation String for an invented predicate clause.
%
clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_,Es):-
	once(clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_,[],Es)).

%!	clause_explanation(+Sym,+Syms,+Ops,+Prog,+Inv,+Known,-New,+Acc,-String)
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
%	an explanation for. Inv is the list of invented symbols (the
%	"invented signature") used to determine which litearls in the
%	clause's body to explain (we don't want to explain non-invented
%	literals!).
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
clause_explanation(_S,[],[],_Ps,_Is,Ks,Ks,Es,Es).
% A body literal has an invented symbol and might need explanation.
clause_explanation(S,[Si|Ss],Os,Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	memberchk(Si,Is)
	,start_of_definition(Si,Ps,Ds)
	,predicate_explanation(Si,Ds,Is,Ks,Ks_,Si-E,Ps_)
	,clause_explanation(S,[E|Ss],Os,Ps_,Is,Ks_,Ks_Bind,Acc,Bind).
clause_explanation(S,[S1,S2|Ss],Os,Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	memberchk(S1,Is)
	,start_of_definition(S1,Ps,Ds)
	,predicate_explanation(S1,Ds,Is,Ks,Ks_,S1-E,Ps_)
	,clause_explanation(S,[E,S2|Ss],Os,Ps_,Is,Ks_,Ks_Bind,Acc,Bind).
clause_explanation(S,[S1,S2|Ss],Os,Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	memberchk(S2,Is)
	,start_of_definition(S2,Ps,Ds)
	,predicate_explanation(S2,Ds,Is,Ks,Ks_,S2-E,Ps_)
	,clause_explanation(S,[S1,E|Ss],Os,Ps_,Is,Ks_,Ks_Bind,Acc,Bind).
% Explanation operators with blank expressions.
clause_explanation(S,[Si|Ss],[O|Os],Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	O =.. [_|['']]
	,clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_Bind,[Si|Acc],Bind).
clause_explanation(S,[S1,S2|Ss],[infix('')|Os],Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_Bind,[S1,S2|Acc],Bind).
% Explanation operators dealing with recursion.
clause_explanation(S,[S|Ss],[prefix(E)|Os],Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	configuration:recursion_explanation(R)
	,clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_Bind,[E,R|Acc],Bind).
clause_explanation(S,[S,S2|Ss],[infix(E)|Os],Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	configuration:recursion_explanation(R)
	,clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_Bind,[R,E,S2|Acc],Bind).
clause_explanation(S,[S1,S|Ss],[infix(E)|Os],Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	configuration:recursion_explanation(R)
	,clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_Bind,[S1,E,R|Acc],Bind).
clause_explanation(S,[S|Ss],[suffix(E)|Os],Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	configuration:recursion_explanation(R)
	,clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_Bind,[R,E|Acc],Bind).
% Nice, uncomplicated explanation operators.
clause_explanation(S,[Si|Ss],[prefix(E)|Os],Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_Bind,[E,Si|Acc],Bind).
clause_explanation(S,[S1,S2|Ss],[infix(E)|Os],Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_Bind,[S1,E,S2|Acc],Bind).
clause_explanation(S,[Si|Ss],[suffix(E)|Os],Ps,Is,Ks,Ks_Bind,Acc,Bind):-
	clause_explanation(S,Ss,Os,Ps,Is,Ks,Ks_Bind,[Si,E|Acc],Bind).


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
literals_symbols(Ls,Ss):-
	findall(S
	       ,(member(L,Ls)
		,functor(L,S,_A)
		)
	       ,Ss).


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
clause_metarule(C,Ss,M):-
	named_metarule(M,metarule(Ss,Ls))
	,metarule_clause(metarule(Ss,Ls),C).


%!	metarule_clause(+Metarule,+Clause) is det.
%
%	True when Clause is a clause of the given Metarule.
%
metarule_clause(M,(H:-Bs)):-
	metarule_literals(M,Ls_)
	,copy_term((H:-Bs),(H_:-Bs_))
	,numbervars((H_:-Bs_))
	,once(list_tree(Ls_,(H_,Bs_))).


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
metarule_literals(metarule(_,Ls),Ls_):-
	metarule_literals(Ls,[],Ls_).

%!	metarule_literals(+Metarule,+Acc,-Literals) is det.
%
%	Business end of metarule_literals/2.
%
metarule_literals([],Acc,Ls):-
	!
	,reverse(Acc,Ls).
metarule_literals((H:-Bs),Acc,Bind):-
% H is a list of terms, Bs is a list of lists of terms
	!
	,H_ =.. H
	,metarule_literals(Bs,[H_|Acc],Bind).
metarule_literals([L|Ls],Acc,Bind):-
% Should also cover unit clauses.
	!
	,L_ =.. L
	,metarule_literals(Ls,[L_|Acc],Bind).


%!	predicate_explanation(+Sym,+Program,+Invented,+Known,-New,-Explanation,-Rest)
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
%	Program is a list of clauses and Invented is a list of invented
%	symbols. Explanations is a key-value pair, S-E, where S the
%	input Symbol and E is the program explanation of the predicate.
%
%	Known and New are the list of predicate explanations obtained
%	so-far, and the same list augmented with the explanation of the
%	current predicate.
%
predicate_explanation(S,Ps,Is,Ks,Ks_,S-E,Rs):-
	interactive_session(false)
	,!
	,predicate_explanation_(S,Ps,Is,Ks,Ks_,S-E,Rs).
predicate_explanation(S,Ps,Is,Ks,Ks2,S-UE,Rs):-
	interactive_session(true)
	,predicate_explanation_(S,Ps,Is,Ks,Ks1,S-E,Rs)
	,user_explanation(S-E,Ks1,S-UE,Ks2).


%!	predicate_explanation_(+Sym,+Program,+Invented,+Known,-New,-Explanation,-Rest)
%!	is det.
%
%	Business end of predicate_explanation/7.
%
%	Split off to allow predicate_explanation/7 to start an
%	interactive user session if required.
%
predicate_explanation_(S,Ps,_Is,Ks,Ks,S-E,Ps):-
	memberchk(S-E,Ks)
	,!.
predicate_explanation_(S,Ps,Is,Ks,Ks_,PE,Rs):-
	predicate_explanation(S,Ps,Is,Ks,Ks_,[],PE,Rs).

%!	predicate_explanation(+Sym,+Prog,+Inv,+Known,-New,+Acc,-Explanations,-Rest)
%!	is det.
%
%	Business end of predicate_explanation/6.
%
%	Acc is the accumulator for clause explanations that are used to
%	form the predicate explanation for the predicate with the given
%	Sym(bol).
%
predicate_explanation(S,[C|Ps],Is,Ks,Ks_Bind,Acc_Es,Bind_Es,Bind_Rs):-
	explained_clause(C,Ps,Is,Ks,Ks_,S-E)
	,!
	,predicate_explanation(S,Ps,Is,Ks_,Ks_Bind,[E|Acc_Es],Bind_Es,Bind_Rs).
predicate_explanation(S,Ps,_Is,Ks,[S-E|Ks],Acc_Es,S-E,Ps):-
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
explained_literals([],_Es,Acc,Ls):-
	reverse(Acc,Ls)
	,!.
explained_literals([L|Ls],Es,Acc,Bind):-
	L =.. [S|As]
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
