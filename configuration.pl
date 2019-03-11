:-module(configuration, [explanation_connectives/1
			,explanation_operators/2
			,named_metarule/2
			,recursion_explanation/1
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


%!	named_metarule(?Name,?Metarule) is semidet.
%
%	A named metarule in Metagol syntax.
%
named_metarule(identity,metarule([P,Q],([P,A,B]:-[[Q,A,B]]))).
named_metarule(inverse,metarule([P,Q],([P,A,B]:-[[Q,B,A]]))).
named_metarule(chain,metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]]))).


%!	recursion_explanation(?String) is semidet.
%
%	Explanation String for recursive literals.
%
%	The symbol of a recursive literal, i.e. one with the same symbol
%	as the head literal in a clause, will be renamed to the given
%	String during clause explanation.
%
recursion_explanation(this).
