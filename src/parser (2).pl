:- module(term, [
	atomic_expression/3,
	compound_expression/3,
	expression/3,
	statement/3,
	function/3,
	program/3,
	parse/2
]).

:- use_module(lexer).
:- set_prolog_flag(double_quotes, chars).

% parse/2 - initialize the parsing of a program
parse(W, AST) :- tokenize(W, Tokens), phrase(program(AST), Tokens), !.

/** Plurals */
functions([D]) --> function(D).
functions([D | Ds]) --> function(D), functions(Ds).

statements([S]) --> statement(S).
statements([S | Ss]) --> statement(S), ',', statements(Ss).

expressions([X]) --> expression(X).
expressions([X | Xs]) --> expression(X), ',', expressions(Xs).

names([N]) --> name(N).
names([N | Ns]) --> name(N), ',', names(Ns).

/** Optionally parenthesized list of names */
name_s([N]) -->  name(N).
name_s(Ns)  --> '(', names(Ns), ')'.

/** Program */
program(program(Ds)) --> functions(Ds).

/** Variants of functions: */
function(function(N, Is, Os, B)) --> name(N), formals(Is, Os), body(B, (Is, Os)).

formals(Is, Os) -->  Is = ["#"], Os = ["$"].	% Default names 
formals(Is, Os) -->  inputs(Is), outputs(Os).	%   

inputs(Is) -->  name_s(Is).  

outputs(Os) -->  Os = ["$"].     	  % Unnamed output 
outputs(Os) -->  name_s(Is).              % Named output(s)


body(B) --> '{', statements(B), '}'.

/** Statement variants */
statement(term(T)) --> term(T).              		% Every Prolog like term is a valid statement.
statement(assignment(A)) --> assignment(A).  		% In addition, assignment to variables (binding). 
statement(statements(Ss)) --> '{', statements(Ss), '}'. % Block statement 
statement(return(X)) --> invocation(X).

term(term(N, As)) --> funktor(F), actuals(As).

actuals(Xs)  --> '(', expressions(Xs), ')'. 

/** Assignment */

assignment((T, F)) --> into(T), '<-', from(F).

into(Ns) -->  name_s(Ns).

from(F) --> invocation(F).

invocation(invocation(N, As)) --> name(N), actuals(As).


/** Expression */
expression(X) --> atomicX(X).
expression(X) --> compoundX(X).

/** Atomic expressions */
atomicX(X) --> literal(X).
atomicX(X) --> name(X).

/** Compound Expressions */ 
compoundX(X) --> wrapped(X).
compoundX(X) --> tuple(X).
compoundX(X) --> invocation(X).
compoundX(X) --> term(X).

wrapped(X) --> '(', expression(X), ')'.

tuple([X | Xs]) --> '(', expression(X), ',', expressions(Xs), ')'.

/** Tokens */

/** Punctuation: */
'(' --> [operator("(")].  ')' --> [operator(")")]. % Wraps expressions
'{' --> [operator("{")].  '}' --> [operator("}")]. % Wraps statements
',' --> [operator(",")]. % Separates expressions
';' --> [operator(";")]. % Separates statements

/** Operators */
'->' --> [operator("->")]. % Function definition
'<-' --> [operator("<-")]. % Assignment

/** Reserved speical identifiers */
special('#') --> '#'.
special('$') --> '$'.
special('_') --> '_'.

'#' --> [operator("#")]. % Special name of input tuple 
'$' --> [operator("$")]. % Special name of output tuple
'_' --> [operator("_")]. % Special name of anonymous name 

/** Literals */ 
literal(number(N)) --> [number(N)].
literal(string(S)) --> [string(S)].

/* Identifiers */
identifier(I) --> lower(I). % Lower case identifiers
identifier(I) --> upper(I). % Upper case identifiers

lower(L) --> [lower(L)]. % lower is a lower case Prolog identifier. 
upper(F) --> [upper(F)]. % upper is an upper case Prolog identifier, 

name(N) --> upper(N).
name(N) --> special(N).

funktor(F) --> lower(F).
