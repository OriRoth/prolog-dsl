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

/**********************
 * Atomic Expressions *
 **********************/

literal(number(N)) --> [number(N)].
literal(string(N)) --> [string(N)].
literal(prolog_id(I)) --> [prolog_id(I)].

variable(function_id(I)) --> [function_id(I)].

atomic_expression(X) --> literal(X).
atomic_expression(X) --> variable(X).

/************************
 * Compound Expressions *
 ************************/

invocation(invocation(prolog_id(I), Xs)) -->
	[prolog_id(I)],
	[operator("(")],
	expressions(Xs),
	[operator(")")].
invocation(invocation(V, Xs)) -->
	variable(V),
	[operator("(")],
	expressions(Xs),
	[operator(")")].

parenthesized(X) -->
	[operator("(")],
	expression(X),
	[operator(")")].

tuple(tuple([X | Xs])) -->
	[operator("(")],
	expression(X),
	[operator(",")],
	expressions(Xs),
	[operator(")")].

compound_expression(X) --> invocation(X).
compound_expression(X) --> parenthesized(X).
compound_expression(X) --> tuple(X).

/***************
 * Expressions *
 ***************/

expression(X) --> atomic_expression(X).
expression(X) --> compound_expression(X).

expressions([X]) --> expression(X).
expressions([X | Xs]) -->
	expression(X),
	[operator(",")],
	expressions(Xs).

/**************
 * Statements *
 **************/

variables([V]) --> variable(V).
variables([V | Vs]) -->
	variable(V),
	[operator(",")],
	variables(Vs).

assignment(assignment([V], X)) -->
	variable(V),
	[operator("=")],
	expression(X).
assignment(assignment(Vs, X)) -->
	[operator("(")],
	variables(Vs),
	[operator(")")],
	[operator("=")],
	expression(X).

imperative_statement(prolog_id(I)) --> [prolog_id(I)].
imperative_statement(I) --> invocation(I).
imperative_statement(A) --> assignment(A).

imperative_statements([S]) --> imperative_statement(S).
imperative_statements([S | Ss]) -->
	imperative_statement(S),
	[operator(",")],
	imperative_statements(Ss).

imperative_block(imperative(Ss)) -->
	[operator("{")],
	imperative_statements(Ss),
	[operator("}")].

return_statement(return(E)) --> expression(E).

statement(Ss) --> imperative_block(Ss).
statement(S) --> return_statement(S).

statements([S]) --> statement(S).
statements([S | Ss]) -->
	statement(S),
	[operator(",")],
	statements(Ss).

/***************
 * Definitions *
 ***************/

definition(definition(Name, Inputs, Body)) -->
	variable(Name),
	[operator("(")],
	variables(Inputs),
	[operator(")")],
	[operator(":-")],
	statements(Body),
	[operator(".")].

/***********
 * Program *
 ***********/

definitions([D]) --> definition(D).
definitions([D | Ds]) -->
	definition(D),
	definitions(Ds).

program(program(Ds)) --> definitions(Ds).

parse(W, AST) :-
	tokenize(W, Tokens),
	phrase(program(AST), Tokens), !.
