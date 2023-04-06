 :- module(term, [
	atomic_expression/3,
	compound_expression/3,
	expression/3,
	statement/3,
	definition/3,
	program/3,
	parse/2
]).
:- use_module(lexer).

parse(W, AST) :-
	tokenize(W, Tokens),
	phrase(program(AST), Tokens), !.

% Tokens 
'{' --> [operator("{")].
'}' --> [operator("}")].
'(' --> [operator("(")].
')' --> [operator(")")].
':-' --> [operator(":-")].
'. ' --> [operator(".")].
', ' --> [operator(",")].
'= ' --> [operator("=")].
'=>' --> [operator("=>")]. 

% Plurals
definitions([D]) --> definition(D).
definitions([D | Ds]) --> definition(D), definitions(Ds).

statements([S]) --> statement(S).
statements([S | Ss]) --> statement(S), ', ' , statements(Ss).

expressions([X]) --> expression(X).
expressions([X | Xs]) --> expression(X), ", ", expressions(Xs).

% expression tuple vs parenthesized expression
tuple(tuple([X|Xs])) --> "(", expression(X), ", ", expressions(Xs), ")".  % A tuple has two or more expressions
parenthesized(X) --> "(", expression(X), ")".                             % Exactly one expression

% slot (a single variable) vs slots (a possible compound set of variables) 
pattern(P) --> slot(P).
pattern(P) --> slots(P).
slots(slots([X | Xs])) --> "(", slot(X), ", ", slots(Xs), ")".  % Slots have two or inner slots 
slot(V) --> "(", slot(V), ")".                                  % Parenthesize slots has exactly one slot. 
slot(V) --> variable(V).                                        % Parentheses are optional



% Program
program(program(Ds)) --> definitions(Ds).

 % definition 
definition(definition(Name, Pattern, Body)) --> header(Name, Pattern), body(Body).

% header
header(Name, Pattern) --> variable(Name), pattern(Pattern).

% body
body(Body) --> ':-', statements(Body), '. '.
statement(Ss) --> imperative_block(Ss).
statement(S) --> return_statement(S).
return_statement(return(E)) --> expression(E).

/**********************
 * Atomic Expressions *
 **********************/

literal(number(N)) --> [number(N)].
literal(string(N)) --> [string(N)].
literal(prolog_id(I)) --> [prolog_id(I)].


atomic_expression(X) --> literal(X).
atomic_expression(X) --> variable(X).

/************************
 * Compound Expressions *
 ************************/

invocation(invocation(prolog_id(I), Xs)) -->
	[prolog_id(I)],"(", expressions(Xs), ")".
invocation(invocation(V, Xs)) -->
	variable(V), "(", expressions(Xs), ")".


compound_expression(X) --> invocation(X).
compound_expression(X) --> parenthesized(X).
compound_expression(X) --> tuple(X).

/***************
 * Expressions *
 ***************/

expression(X) --> atomic_expression(X).
expression(X) --> compound_expression(X).


imperative_statement(prolog_id(I)) --> [prolog_id(I)].
imperative_statement(I) --> invocation(I).
imperative_statement(A) --> assignment(A).

imperative_statements([S]) --> imperative_statement(S).
imperative_statements([S | Ss]) -->
	imperative_statement(S),
	", ",
	imperative_statements(Ss).

imperative_block(imperative(Ss)) --> "{", imperative_statements(Ss), "}".

assignment(assignment(Vs, X)) --> pattern, "=", expression(X).
