:-module(configuration, [explanation_connectives/1
			,explanation_operators/2
			,interactive_session/1
			,interpretation_string/2
			,interpretation_string/4
			,invention_assumption/1
			,named_metarule/2
			,recursion_explanation/1
			,variable_interpretation/1
			]).

/** <module> Configuration options for metasplain library.
*/

%!	explanation_connectives(?Connectives) is semidet.
%
%	Connectives used to connect clauses of an invented predicate.
%
%	Connectives is alist of explanation connectives, atoms of the
%	form connective(C) where C is a word used to connect clauses of
%	an invented predicate.
%
%	Given that invented predicates are conjunctions of clauses, the
%	principal connective that mostly makes sense is "or", but "and",
%	and perhaps others may be desired, depending on the predicate.
%
explanation_connectives([connective(or)]).


%!	explanation_operators(?Metarule,?Operators) is semidet.
%
%	A list of explanation Operators for the named Metarule.
%
%	Metarule is the name of a named metarule, as in
%	named_metarule/2. Operators is a list of explanation operators,
%	atoms of the form Precendence(Expresion) where Precedence is one
%	of [prefix,infix,suffix] and Expression is a natural language
%	expression used to explain the relation between predicates in a
%	clause of the Metarule.
%
%	Expression should be given as a Prolog atom (a constant). It may
%	not include spaces. If spaces are required, the underscore '_'
%	can be used in their stead.
%
explanation_operators(identity,[prefix('')]).
explanation_operators(inverse,[prefix(anti)]).
explanation_operators(chain,[infix(of)]).
explanation_operators(unchain,[infix(of),infix('')]).
explanation_operators(precon,[infix(if)]).
explanation_operators(postcon,[infix(if)]).
explanation_operators(curry_3,[prefix('')]).
explanation_operators(curry_4,[prefix('')]).
explanation_operators(curry_5,[prefix('')]).
explanation_operators(tailrec,[infix(of)]).


%!	interactive_session(?Bool) is semidet.
%
%	Whether to start an interactive user session or not.
%
%	If Bool is true metasplain will pause and prompt the user for an
%	interpretation of automatically derived explanations. If it is
%	false, nothing will happen.
%
%	Note well: the value of Bool is not validated in any way. If
%	Bool is anything other than true or false, explanation will fail
%	silently.
%
interactive_session(true).


%!	interpretation_string(?Metarule,?String) is semidet.
%
%	A String used to interpret a clause of a Metarule.
%
%	The 2-arity version of interpretation_string is used when
%	variable_interpretation(false) is set. When that is the case,
%	variables in clauses' literals are excluded from the
%	interpretation of a program resulting in a string resembling an
%	expression in a natural language.
%
interpretation_string(identity,['A',_P,is,a,_Q]).
interpretation_string(chain,['The',_P,is,the,_Q,of,the,_R]).
interpretation_string(postcon,['A',_P,is,a,_Q,if,the,game,is,_R]).
interpretation_string(unchain,['A',_P,is,a,_Q,that,is,not,followed,by,a,_R]).


%!	interpretation_string(?Metarule,?Second_Order,?First_Order,?String)
%!	is semidet.
%
%	A string used to interpret a clause of a metarule.
%
%	The 4-arity version of interpretation_string is used when
%	variable_interpretation(true) is set. When that is the case,
%	variables in clauses' literals are included to the
%	interpretation of a program.
%
interpretation_string(identity,[P,Q],[A,B],[A,is,the,P,of,B,if,A,is,the,Q,of,B]).
interpretation_string(chain
		     ,[P,Q,R]
		     ,[A,B,C]
		     ,[A,is,the,P,of,B,if,A,is,the,Q,of,C,and,C,is,the,R,of,B]).
interpretation_string(postcon,[P,Q,R]
		     ,[A,B]
		     ,[A,is,the,B,of,P,if,A,is,the,Q,of,B,and,B,is,R]).
interpretation_string(unchain
		     ,[P,Q,not,R]
		     ,[A,B,C]
		     ,[A,is,the,P,of,B,if,A,is,the,Q,of,C,and,C,is,not,the,R,of,B]).


%!	invention_assumption(?Strength) is semidet.
%
%	The Strength of assumptions about invented predicates.
%
%	Strength can be one of [strong,weak]. Both refer to the
%	strength of the assumptions about the naming of invented
%	predicate symbols, in metasplain.
%
%	Under the strong assumption metasplain will consider any legal
%	Prolog atom that ends in an underscore followed by at least one
%	number as an invented predicate symbol.
%
%	Under the weak assumption metasplain will only consider a
%	predicate symbol to be invented if its list of characters can be
%	unified to the list of characters in the top-goal of the
%	processed program, followed by any number of digits preceded by
%	underscores.
%
%	For example, the following tabulates examples of identifiers and
%	their acceptance or not as invented under the strong or weak
%	predicate invention assumption, given that the predicate symbol
%	of the top-goal in the program is "p":
%
%	| Predicate symbol | Strong assumption | Weak assumption |
%	| p                | not invented      | not invented    |
%	| p_1              | invented	       | invented        |
%	| r_1              | invented	       | not invented    |
%	| p_1_2_1          | invented	       | invented        |
%	| p1               | not invented      | not invented    |
%	| p_		   | not invented      | not invented    |
%	| p_aux		   | not invented      | not invented    |
%
%	The motivation for this option is to allow processing of
%	programs where the top-goal is itself an invented predicate. For
%	instance, in the following example, the predicate symbol win_2
%	is the symbol of the top-goal in the program:
%	==
%	win_2(A,B):-win_2_1_1(A,B),not(win_2_1_1(B,C)).
%	win_2_1_1(A,B):-move(A,B),not(win_1(B,C)).
%	win_1(A,B):- move(A,B),won(B)).
%	==
%
%	However, it is not possible to say whether win_2 is itself
%	invented, and therefore, requires explanation, or not. Perhaps
%	the user simply wanted to keep this program separate from a
%	different one, learned (or written) earlier. Other predicate
%	symbols in the program can offer some hint but nothing can be
%	said with certainty.
%
%	Therefore, an assumption must be made about the structure of
%	invented predicate symbols, in order to proceed with
%	explanation. Under the strong assumption win_2 will be
%	considered to be an invented predicate and metasplain will try
%	to explain it, as well as win_2_1_1 and win_1, even though
%	win_1 does not match the start of the top-goal, win_2. Under the
%	weak assumption, metasplain will consider win_2 as not invented
%	and will only attempt to explain win_2_1_1, but not win_1,
%	because its start doesn't match of win_2.
%
invention_assumption(weak).


%!	named_metarule(?Name,?Metarule) is semidet.
%
%	A named metarule in Metagol syntax.
%
named_metarule(identity,metarule([P,Q],([P,A,B]:-[[Q,A,B]]))).
named_metarule(inverse,metarule([P,Q],([P,A,B]:-[[Q,B,A]]))).
named_metarule(chain,metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]]))).
named_metarule(unchain,metarule([P,Q,not,R],([P,A,B]:-[[Q,A,B],[not,[R,B,_C]]]))).
named_metarule(precon,metarule([P,Q,R],([P,A,B]:-[[Q,A],[R,B]]))).
named_metarule(postcon,metarule([P,Q,R],([P,A,B]:-[[Q,A,B],[R,B]]))).
named_metarule(curry_3,metarule([P,Q,R],([P,A,B]:-[[Q,A,B,R]]))).
named_metarule(curry_4,metarule([P,Q,R,S],([P,A,B]:-[[Q,A,B,R,S]]))).
named_metarule(curry_5,metarule([P,Q,R,S,T],([P,A,B]:-[[Q,A,B,R,S,T]]))).
named_metarule(tailrec,metarule([P,Q,P],([P,A,B]:-[[Q,A,C],[P,C,B]]))).


%!	recursion_explanation(?String) is semidet.
%
%	Explanation String for recursive literals.
%
%	The symbol of a recursive literal, i.e. one with the same symbol
%	as the head literal in a clause, will be renamed to the given
%	String during clause explanation.
%
recursion_explanation(this).


%!	variable_interpretation(?Bool) is semidet.
%
%	Whether to include variables to an automatic interpretation.
%
variable_interpretation(false).
