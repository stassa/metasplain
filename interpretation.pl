:-module(interpretation, [interpretation/2
			 ]).

:-use_module(metasplain).

/** <module> Generate program interpretations.
*/

%!	interpretation(+Program,-Interpretation) is det.
%
%	Form an Interpretation for a Program.
%
%	Interpretation is a string of expressions in a Controlled
%	Natural Language, derived from the structure of the program in
%	combination with a number of user-provided interpretation
%	strings.
%
interpretation(Ps,Is):-
	predicate_interpretations(Ps,Is_Cs)
	,program_interpretation(Is_Cs,Is).


%!	predicate_interpretations(+Program,-Interpretations) is det.
%
%	Form Interpretations for all predicates in a Progarm.
%
predicate_interpretations(Ps,Is):-
	predicate_interpretations(Ps,[],Is).

%!	predicate_interpretations(+Program,+Acc,-Interpretations) is
%!	det.
%
%	Business end of predicate_interpretations/2
%
predicate_interpretations([],Acc,Is):-
	reverse(Acc,Is)
	,!.
predicate_interpretations([C|Cs],Acc,Bind):-
	select_definition(C,[C|Cs],Ds,Rs)
	,clause_predicate_symbol(C,S/A)
	,clause_interpretations(S/A,Ds,Is)
	,predicate_interpretations(Rs,[Is|Acc],Bind).


%!	clause_interpretations(+Predicate,+Definition,-Interpretations)
%!	is det.
%
%	Form Interpretations for all clauses of a Predicate.
%
clause_interpretations(S/A,Ds,Is):-
	clause_interpretations(S/A,Ds,[],Is).

%!	clause_interpretations(+Predicate,+Definition,+Acc,-Interpretations)
%!	is det.
%
%	Business end of clause_interpretations/3
%
clause_interpretations(S/A,[],Acc,S/A-Is):-
	reverse(Acc,Is)
	,!.
clause_interpretations(S/A,[C|Cs],Acc,Bind):-
	configuration:variable_interpretation(false)
	,!
	,clause_literals(C,Ls)
	,literals_symbols(Ls,Ss)
	,once(clause_metarule(C,Ss,M))
	,configuration:interpretation_string(M,Is)
	,once(phrase(clause_interpretation(Is),Ss))
	,cleaned_interpretation(Is, Is_)
	,clause_interpretations(S/A,Cs,[Is_|Acc],Bind).
clause_interpretations(S/A,[C|Cs],Acc,Bind):-
	configuration:variable_interpretation(true)
	,clause_literals(C,Ls)
	,literals_symbols(Ls,Ss)
	,once(clause_metarule(C,Ss,M))
	,term_variables(Ls,Vs)
	% Avoid binding variables in other clauses.
	% Because they're usually not standardised apart!
	,copy_term(Vs,Vs_)
	,numbervars(Vs_)
	,configuration:interpretation_string(M,Ss,Vs_,Is)
	,varnumbers(Is,Is_1)
	,cleaned_interpretation(Is_1, Is_2)
	,clause_interpretations(S/A,Cs,[Is_2|Acc],Bind).


%!	clause_interpretation(?Symbols) is nondet.
%
%	Interpret a list of predicate Symbols.
%
clause_interpretation([]) --> [].
clause_interpretation([S|Ss]) --> [S], clause_interpretation(Ss).
clause_interpretation([_S|Ss]) --> clause_interpretation(Ss).


%!	cleaned_interpretation(+Interpretation,-Cleaned) is det.
%
%	Cleanup an Interpretation string.
%
%	Currently, this just removes underscores between symbols'
%	substrings.
%
cleaned_interpretation(Is,Cs):-
	cleaned_interpretation(Is,[],Cs).

%!	cleaned_interpretation(+Interpretation,+Acc,-Cleaned) is det.
%
%	Business end of cleaned_interpretation/2.
%
cleaned_interpretation([],Acc,Cs):-
	reverse(Acc,Cs_)
	,flatten(Cs_,Cs).
cleaned_interpretation([S|Is],Acc,Bind):-
	atom(S)
	,!
	,atomic_list_concat(Ss,'_',S)
	,cleaned_interpretation(Is,[Ss|Acc],Bind).
cleaned_interpretation([S|Is],Acc,Bind):-
	cleaned_interpretation(Is,[S|Acc],Bind).


%!	program_interpretation(+Program,-Interpretation) is det.
%
%	Form an Interpretation for a Program.
%
program_interpretation(Ps,Is):-
	program_interpretation(Ps,[],Is).

%!	program_interpretation(+Program,+Acc,-Interpretation) is det.
%
%	Business end of program_interpretation/2.
%
program_interpretation([],Acc,Is):-
	reverse(Acc,Is)
	,!.
program_interpretation([_S/_A-[Is]|Ps],Acc,Bind):-
	!
	,numbervars(Is)
	,findall(A
		,(member(T,Is)
		 ,format(atom(A),'~w',[T])
		 )
		,As)
	,atomic_list_concat(As,' ',I)
	,program_interpretation(Ps,[I|Acc],Bind).
program_interpretation([_S/_A-Is|Ps],Acc,Bind):-
	configuration:explanation_connectives([connective(C)])
	,atomic_list_concat([' ',C,' '],'',C_)
	,merged_interpretations(Is,Ms)
	,numbervars(Ms)
	% Could generalise...
	,findall(As
		,(member(Ts,Ms)
		 ,findall(A
			 ,(member(T,Ts)
			  ,format(atom(A),'~w',[T])
			  )
			 ,As)
		 )
		,AS)
	,findall(CI_
		,(member(CI,AS)
		 ,atomic_list_concat(CI,' ',CI_)
		 )
		,Cs)
	,atomic_list_concat(Cs,C_,Is_)
	,program_interpretation(Ps,[Is_|Acc],Bind).


%!	merged_interpretations(+Interpretations,-Merged) is det.
%
%	Merge a list of Interpretations, removing repeating tokens.
%
merged_interpretations([I|Is],[I|Ms]):-
	merged_interpretations(I,Is,[],Ms).

%!	merged_interpretations(+First,+Interpretations,+Acc,-Merged) is
%!	det.
%
%	Business end of merged_interpretations/2.
%
merged_interpretations(_,[],Acc,Ms):-
	reverse(Acc,Ms)
	,!.
merged_interpretations(I1,[I2|Is],Acc,Bind):-
	string_difference(I1,I2,Ds)
	,merged_interpretations(I1,Is,[Ds|Acc],Bind).


%!	string_difference(+String_1,+String_2,-Difference) is det.
%
%	The Difference of two strings with a common prefix.
%
string_difference(Ss1,Ss2,Ds):-
	string_difference_(Ss1,Ss2,Ds).

%!	string_difference_(+String_1,+String_2,+Acc,-Difference) is det.
%
%	Business end of string_difference/3
%
string_difference_([T|Ss1],[T|Ss2],Bind):-
	!
	,string_difference_(Ss1,Ss2,Bind).
string_difference_(_Ss1,Ss,Ss).
