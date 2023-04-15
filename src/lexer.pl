:- module(lexer, [
	white/2,
	comment/2,
	skip/2,
	operator/3,
	number/3,
	string/3,
	tuple_id/3,
	predicate_id/3,
	variable_id/3,
	tokenize/2
]).

tokenize(Stream, Tokens) :-
	string_chars(W, Characters),
	phrase(tokens(Tokens), Characters), 
  !.

tokens([])          -->  skip.                                         
tokens([T|Ts])      -->  skip,  operator(T),            tokens(Ts).    
tokens([T])         -->  skip,  token_no_operator(T).                  
tokens([T|Ts])      -->  skip,  token_no_operator(T),   skip,          tokens(Ts).
tokens([T1,T2|Ts])  -->  skip,  token_no_operator(T1),  operator(T2),  tokens(Ts).

skip   -->  '';      (white|comment),skip.                
white  -->  '\x20';  '\t'; '\n';  '\f';  '\r'.
comment --> '/*',sequence(Contents),'*/',
	{
		str_type(Contents, ascii),
		\+ occurs('*/', Contents)
	}.


% Operators *
operator(operator("(")) --> ['('].
operator(operator(")")) --> [')'].
operator(operator("{")) --> ['{'].
operator(operator("}")) --> ['}'].
operator(operator(":-")) --> [':'], ['-'].
operator(operator(".")) --> ['.'].
operator(operator(",")) --> [','].
operator(operator("=")) --> ['='].

/***************
 * Identifiers *
 ***************/
function_id(function_id(Identifier)) --> sequence(Characters),
	{
		Characters = [First | Rest],
		char_type(First, prolog_var_start),
		str_type(Rest, prolog_identifier_continue),
		string_chars(Identifier, Characters)
	}.

tuple_id(tuple_id(Identifier)) --> sequence(Characters),
	{
		Characters = ['#' | Rest],
		char_type(First, prolog_atom_start),
		str_type(Rest, prolog_identifier_continue),
		string_chars(Identifier, Characters)
	}.

prolog_id(prolog_id(Identifier)) --> sequence(Characters),
	{
		Characters = [First | Rest],
		char_type(First, prolog_atom_start),
		str_type(Rest, prolog_identifier_continue),
		string_chars(Identifier, Characters)
	}.


/*************
 * Utilities *
 *************/

sequence([]) --> [].
sequence([E | Es]) --> [E], sequence(Es).

starts_with([], _).
starts_with([C | Ys], [C | String_Rest]) :-
	starts_with(Sub_Rest, String_Rest), !.
starts_with(Sub, [_ | String_Rest]) :- starts_with(Sub, String_Rest), !.

occurs(Sub, Str) :- starts_with(Sub, Str), !.
occurs(Sub, [_, Str]) :- occurs(Sub, Str), !.

str_type([], _).
str_type([First | Rest], Type) :-
	char_type(First, Type),
	str_type(Rest, Type).

% Skips 
 

/************
 * Literals *
 ************/

number(number(Number)) --> sequence(Characters),
	{
		string_chars(String, Characters),
		number_string(Number, String)
	}.

string(string(String)) --> ['"'], sequence(Characters), ['"'],
	{
		str_type(Characters, ascii),
		\+ occurs(['"'], Characters),
		\+ occurs(['\n'], Characters),
		string_chars(String, Characters)
	}.

/**********
 * Tokens *
 **********/
 
token_no_operator(T) -->
	  number(T)
	| string(T)
	| function_id(T)
	| prolog_id(T).


