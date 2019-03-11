:-module(interactive,[user_explanation/4]).

/** <module> Manage an interactive explanation session.
*/

%!	user_explanation(+Explanation,+Known,-Interpretation,-New) is
%!	det.
%
%	Elicit a user Interpretation of an automatic Explanation.
%
%	Allows the user to re-interpret automatically formed
%	explanations that may be overly-long, or otherwise hard to
%	unterstand.
%
%	Explanation is a term S-E, where S the symbol of an invented
%	predicate and E the explanation formed for that predicate by
%	metasplain (in particular, predicate_explanation/7 and friends).
%
%	Known is a list of known predicate explanations, possibly
%	including known user-provided explanations.
%
%	Interpretation is a user-provided interpretation of E obtained
%	from the user during an interactive session started by this
%	predicate. The interactive session takes the form of a top-level
%	text-based dialogue. The session proceeds as follows:
%
%	1) The system presents the user with the explanation in
%	Automatic and prompts them for a new name for it.
%	2) If the user inputs an invalid interpreation, the user is
%	warned and prompted again.
%	3) Finally, the systems collects the user's input on the prompt.
%
%	An interpretation is "invalid" if it is not a valid Prolog
%	predicate symbol (i.e. a predicate atom).
%
%	A user interpretation is allowed to contain spaces. These will
%	be replaced by underscores in the final explanation.
%
%	A newline terminates the session and causes the system to retain
%	the current explanation in Automatic.
%
%	New is the list Known updated with a term user-Explanation,
%	where Explanation is the explanation proposed by the user. It is
%	used to ensure that the user is not asked to re-interpret a
%	previously provided explanation.
%
user_explanation(S-E,Ks,S-E,Ks):-
	memberchk(user-E,Ks)
	,!.
user_explanation(S-E,Ks,S-UE,[user-UE|Ks_]):-
	user_input(E,UE)
	,selectchk(S-E,Ks,S-UE,Ks_).


%!	user_input(+Explanation,-Input) is det.
%
%	Start an interactive session to collect user Input.
%
user_input(E,Is_):-
	format('How should I explain ~w?~n',[E])
	,read_line_to_codes(user_input,Is)
	,cleaned_input(Is,E,Is_)
	,!
	,format('I will explain ~w as ~w~n~n',[E,Is_]).
user_input(E,Is):-
	format('Sorry, that\'s not a valid name.~n~n',[])
	,user_input(E,Is).


%!	cleaned_input(+Input,+Explanation,-Cleaned) is det.
%
%	Ensure Input is a valid Prolog predicate symbol.
%
cleaned_input([],A,A):-
	!.
cleaned_input(Is,_,As_2):-
	atom_codes(A,Is)
	,atomic_list_concat(As,' ',A)
	,clean_spaces(As,As_1)
	,valid_name(As_1)
	,atomic_list_concat(As_1,'_',As_2).


%!	clean_spaces(+Atoms,-Cleaned) is det.
%
%	Remove spaces from a list of Atoms.
%
clean_spaces(As,As_):-
	findall(A
	       ,(member(A,As)
		,A \= ''
		)
	       ,As_).


%!	valid_name(+Atoms) is det.
%
%	True if the list Atoms can form a valid Prolog atom.
%
valid_name([A|As]):-
	valid_start(A)
	,valid_rest(As).


%!	valid_start(+Atom) is det.
%
%	True if Atom is a valid start of a Prolog atom.
%
%	Checks that the first characters in the user's input are a valid
%	start for a Prolog atom. Note that Atom may include more than
%	one character, e.g. '_a' (which is not a valid atom start) etc.
%
valid_start(A):-
	atom_chars(A,[C|_Cs])
	,char_type(C,prolog_atom_start).


%!	valid_rest(+Atoms) is det.
%
%	True if Atoms can form a valid Prolog atom.
%
%	Checks that remaining characters in the input are valid Prolog
%	predicate symbol characters.
%
valid_rest(As):-
	forall(member(A,As)
	       ,(atom_chars(A,Cs)
		,forall(member(C,Cs)
		       ,char_type(C,prolog_identifier_continue))
		)
	      ).
