 :- module(term, [
	parse/2,
	program/3,
	tupler/3,
	expression/3
]).
:- use_module(lexer).

parse(W, AST) :-
	tokenize(W, Tokens),
	phrase(program(AST), Tokens), !.

% Some simple predicate on lists.
any(L)        :-  L  =  _.
empty(L)      :-  L  =  [].
one(L)        :-  L  =  [_].
two(L)        :-  L  =  [_,_].
not_empty(L)  :-  L  =  [_|_].
three(L)      :-  L  =  [_,_,_,_].
four(L)       :-  L  =  [_,_,_,_,_].

% Plurals
tuplers(Tuplers)        -->  tupler(T),!,tuplers(Ts),             Tuplers=[T|Ts]; '',  Tuplers=[].
parameters(Parameters)  -->  parameter(P),!,',',parameters(Ps),  Parameters=[P|Ps]; '',  Parameters=[].
components(Components)  -->  component(C),!,',',components(Cs),  Components=[C|Cs]; '',  Components=[].

% A list of tuplers comprises a program. A tupler is a triple compirsed by
% name, parameters, and body. The body is a tuple of zero or more expressions
% to be computed and retuned as a tuple when the tupler is invoked on specific
% values of the parameters. 
program(Tuplers)                -->  tuplers(Tuplers).                      
tupler((Name,Parameters,Body))  -->  header(Name,Parameters),               body(Body).
header(Name,Parameters)         -->  tupletor(Name),                        parameters(Parameters).
body(Return)                    -->  ':-',components(Cs),'.',               assemble(Cs,Return)
                                  |  '.', {unit(Return)}.

% A tuple is a pair of constituents and auxiliaries. Both elements two possibly
% empty lists, the elements and the body. A tuple is nullaryItems of both lists
% may be either components or other tuples. 
%
% The first list is of the tuple <em>elements</em>. The other list is of
% <em>hidden</em> tuples, which are not part of the values comprising the
% tuple, but are computed (collaterally) as part of the computation of the
% tuple.

% Tuples come in different varieties.
unit(I)       :-  I=t(Elements,Hidden),  empty(Elements),  empty(Hidden).
procedure(P)  :-  P=t(Elements,Hidden),  empty(Elements),  not_empty(Hidden).
singleton(S)  :-  S=t(Elements,Hidden),  one(Elements),    empty(Hidden).
function(F)   :-  F=t(Elements,Hidden),  one(Elements),    not_empty(Hidden).
pair(S)       :-  S=t(Elements,Hidden),  two(Elements),    empty(Hidden).
triple(T)     :-  T=t(Elements,Hidden),  three(Elements),  empty(Hidden).

tuple(t(Elements,Hidden))   -->  tuple(Elements,Hidden). %tuple/1
tuple(Elements,Hidden)      -->  '{',components(Cs),'}',   Elements=Cs,              Hidden=[] % tuple/2
                              |  '(',components(Cs),')',   assemble(Cs,Tuple), Tuple=t(Elements, Hidden).
assemble(Components,Tuple)  :-   classify(Components,Elements,Hidden),  Tuple=(Elements,Hidden).


% An expression is either a tuple, or a component.
expression(t(Elements,Hidden))  -->  tuple(Elements,Hidden).
expression(c(C))                -->  component(C).

assignment(a(Pattern, X))       --> pattern(Pattern),':=',expression(X).
/*

#solve(A, B, C) :-
	{ Delta := #subtract (#square B, #mul 4 A C) => #sqrt }
	#div (#plus  (#neg B) Delta) (#mul 2 A),
	#div (#minus (#neg B) Delta) (#mul 2 A).

#gcd(M, N) :-
	#equal M N ?
		M
	: #gt M N ?
		#gcd (#subtract M N) N
	: #gcd (M, #subtract N M).

#compile FileName :-
	FileName,
	SymbolTable,
	#prolog AST SymbolTable ^ ERROR,
	#sml AST SymbolTable ^ ERROR,
	{ SymbolTable := #weave AST,
	  AST := (#read FileName) ^ ERROR
	  => #lex ^ ERROR => #parse ^ ERROR }.

#solve(A, B, C) :-
	{ Delta := #subtract (#square B, #mul 4 A C) },
	{ TwoA := #mul 2 A, NegB := #neg B},
	#gt 0 Delta ? ^^^ : Solutions,
	#div2A X :- #div X TwoA,
	#div2A2 X Y :- X => #div2A, #dic2A Y,
	{ Solutions := #div2A2 (#plus NegB Delta) (#minus NegB Delta) }.

#solve(A, B, C) :-
	{ Delta := #subtract (#square B, #mul 4 A C) => #sqrt },
	{ TwoA := #mul 2 A, NegB := #neg B},
	#gt 0 Delta ? ^^^ : Solutions,
	#div2A X :- #div X TwoA,
	#div2A2 X Y :- X => #div2A, #dic2A Y,
	{ Solutions := #div2A2 (#plus NegB Delta) (#minus NegB Delta) }.

#compile(FileName) :- (FileName, SymbolTable, PrologProgram, SMLProgram),
	{
		Contents := #read FileName,
		Tokens := #lex Contents,
		AST := #parse Tokens,
		SymbolTable := #weave AST,
		PrologProgram := #prolog AST SymbolTable,
		SMLProgram := #sml AST SymbolTable
	}.

#compile(FileName) :- (FileName, SymbolTable, PrologProgram, SMLProgram),
	{
		Contents := #read FileName,
		Tokens, Vocabulary := #lex Contents,
		(Tokens, Vocabulary) := #lex Contents,
		(Tokens Vocabulary) := #lex Contents,
		Tokens Vocabulary := #lex Contents,
		#inspect Tokens Vocabulary,
		AST := #parse Tokens,
		SymbolTable := #weave AST,
		PrologProgram := #prolog AST SymbolTable,
		SMLProgram := #sml AST SymbolTable
	}.

#insights Tokens Vocabulary :- (),
	{
		
	}.

#inspect Tokens Vocabulary :- ()
	{
		#lt (#len Vocabulary) 50 : #throw ERROR
	}.

#inspect Tokens Vocabulary :- { #lt (#len Vocabulary) 50 : #throw ERROR }.

#inspect Tokens Vocabulary :- #lt (#len Vocabulary) 50 : #throw ERROR.

#compile(FileName) :-
	(
		FileName,
		SymbolTable,
		PrologProgram,
		SMLProgram
	), {
		Contents := #read FileName,
		#lex Contents,
		#inspect @Tokens @Vocabulary,
		#insights @Vocabulary,
		AST := #parse Tokens,
		SymbolTable := #weave AST,
		PrologProgram := #prolog AST SymbolTable,
		SMLProgram := #sml AST SymbolTable
	}.

#insights Vocabulary :- { AverageTokenLength := #div (#reduce 0 #plus Vocabulary) (#len Vocabulary) }.

#insights _ :- { AverageTokenLength := #div (#reduce 0 #plus &) (#len &) }.

#insights _ _ :- { AverageTokenLength := #div (#reduce 0 #plus #1) (#len #2) }.

#insights (_, _) :- { AverageTokenLength := #div (#reduce 0 #plus #1) (#len #2) }.

#foo (_, (A, _)) :- { #bar #1, #bar A, #bar #2#1 }.

 */

component(X)   -->  expression(X).
component(X)   -->  elementary(X).
elementary(X)  -->  assignment(X).
elementary(X)  -->  atomic(X).

atomic(I)  -->  invocation(I).
atomic(A)  -->  assignment(A).
atomic(V)  -->  variable(V).
atomic(L)  -->  literal(L).


pipe(p(Source,Filter))  -->  elementary(Source),'=>',filter(Filter).
filter([I])             -->  invocation(I).
filter([I|Is])          -->  invocation(I),'=>',filter(Is).


invocation(invocation(Name, Parameters)) --> name(Name), expression(Parameters).
% #foo (a, A, #add(B, #times C 2))
% #take 2 apples

% slot (a single variable) vs slots (a possible compound set of variables) 
pattern(V)  --> variable(V).
pattern(P)  --> '(', patterns(P), ')'.                          


literal(number(N)) --> [number(N)].
literal(string(N)) --> [string(N)].
literal(prolog_id(I)) --> [prolog_id(I)].
