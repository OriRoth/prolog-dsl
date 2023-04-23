:- module(lexer, [
	tokenize/2,
	number/3,
	string/3,
	tupletor/3,
	predicate/3,
	variable/3
]).

tokenize(Stream, Tokens) :- string_chars(Stream, Characters), phrase(tokens(Tokens), Characters), !.

tokens([])          -->  skip.                
tokens([T])         -->  skip,  token(T).     
tokens([T1,T2|Ts])  -->  skip,  pair(T1,T2),  tokens(Ts).

token(T)         -->  identifier(T)    |  literal(T)    |  punctuation(T).
punctuation(T)   -->  ligature(T),!    |  singleton(T).
literal(L)       -->  number(L)        |  string(L).
identifier(I)    -->  alphanumeric(I)  |  symbolic(I).     
alphanumeric(I)  -->  variable(I)      |  predicate(I)    |  tupletor(I).

arithmetical  -->  '+'   |  '-'  |  '*'   |  '/'.
comparisonal  -->  '<'   |  '>'  |  '='.
singleton(C)  -->  character(C, ":.,?|").
ligature(L)   -->  ":-", L=":-".
programming   -->  '^'   |  '‘'  |  '~'   |  '@'   |  '#'   |  '$'  |  '&'.
backslash     -->  '\\'  .

comment  -->  '/*',!,sequence(Contents),'*/',  {str_type(Contents,ascii),  \+occurs('*/',Contents)}
/**/     |    '%',sequence(_),newline.                                     
skip     -->  (white|comment),!,skip                                         
/**      or   */                               |                           ''.

white    -->  space     |  newline   |  blank  |  tab.
space    -->  '\x20' .
tab      -->  '\t' .
blank    -->  '\f'      |  '\v'.
newline  -->  '\r\n',!  |  '\n\r',!  |  '\n'   |  '\r'.

% Operators *
operator(operator("("))   -->  ['('].
operator(operator(")"))   -->  [')'].
operator(operator("{"))   -->  ['{'].
operator(operator("}"))   -->  ['}'].
operator(operator(":-"))  -->  [':'],  ['-'].
operator(operator("."))   -->  ['.'].
operator(operator(","))   -->  [','].
operator(operator("="))   -->  ['='].

% Identifiers *
predicate(predicate(Id))   -->  predicateStart(First),!,  continue(First,Id).                     
variable(variable(Id))     -->  variableStart(First),!,   continue(First,Id).                     
tupletor(tupletor(Id))     -->  tupletorStart(First),!,   continue(First,Id).                     
predicateStart(Character)  -->  Character,                char_type(C,prolog_atom_start),         !.
variableStart(Character)   -->  Character,                char_type(Character,prolog_var_start),  !.
tupletorStart(Character)   -->  Character,                Character='#',                          !.
continue(First,Id)         -->  rest(Rest),               {compose(First,Rest,Id)}.               
rest(Rest)                 -->  Character,                alphanumeric(Character),!,Rest=[Character,R],       rest(R)
/**/                       |    [].                                                               


/*************
 * Utilities *
 *************/

sequence(L)          -->  L=[]                     |  L=[E|Es],[E],   sequence(Es).
prefix(Prefix,Text)  :-   append(Prefix,_,Text),!  .
occurs(Search,Text)  :-   prefix(Search,Text),!    |  Text=[_,Rest],  occurs(Search,Rest),!  .

str_type([], _).
str_type([First | Rest], Type) :-
	char_type(First, Type),
	str_type(Rest, Type).

% Skips

number(number(Number)) --> sequence(Characters), {string_chars(String,Characters),number_string(Number, String)}.
string(string(String)) --> ['"'], sequence(Contents), ['"'],
	{
		str_type(Contents, ascii),
		\+ occurs(['"'], Contents),
		\+ occurs(['\n'], Contents),
		string_chars(String, Contents)
	}.
