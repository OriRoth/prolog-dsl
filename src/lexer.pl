:- module(lexer, [
	tokenize/2,
	number/3,
	string/3,
	tupletor/3,
	functor/3,
	variable/3
]).

tokenize(Stream, Tokens) :- string_chars(Stream, Characters), phrase(tokens(Tokens), Characters), !.

tokens([])          -->  skip.                
tokens([T])         -->  skip,  token(T).     
tokens([T1,T2|Ts])  -->  skip,  pair(T1,T2),  tokens(Ts).


token(T)       -->  literal(T)  |  identifier(T).     
literal(L)     -->  number(L)   |  string(L).         
identifier(I)  -->  functor(I)  |  variable(I)     |  tupletor(I).


comment  -->  '/*',!,sequence(Contents),'*/',  {str_type(Contents,ascii),  \+occurs('*/',Contents)}
/**/     |    '%',sequence(_),newline.                                     
skip     -->  ''                                                           
/**/     |    (white|comment),skip.                                        

white    -->  space     |  newline   |  blank  |  tab.
space    -->  '\x20' .                             
tab      -->  '\t' .                               
blank    -->  '\f'      |  '\v'.                  
newline  -->  '\r\n',!  |  '\n\r',!  |  '\n'   |  '\r'.

arithmetical  -->  '+'   |  '-'  |  '*'   |  '/'.                      
comparisonal  -->  '<'   |  '>'  |  '='.                               
punctualtion  -->  ':'   |  '.'  |  ','   |  '?'   |  ';'.             
programming   -->  '^'   |  '‘'  |  '~'   |  '@'   |  '#'   |  '$'  |  '&'.
backslash     -->  '\\'  .                                             

% Operators *
operator(operator("("))   -->  ['('].  
operator(operator(")"))   -->  [')'].  
operator(operator("{"))   -->  ['{'].  
operator(operator("}"))   -->  ['}'].  
operator(operator(":-"))  -->  [':'],  ['-'].
operator(operator("."))   -->  ['.'].  
operator(operator(","))   -->  [','].  
operator(operator("="))   -->  ['='].  

/***************
 * Identifiers *
 ***************/
functor(functor(Id))    -->  lower(First),!,    rest(Rest),  {string_chars(Id,[First|Rest])}.
variable(variable(Id))  -->  upper(First),!,    rest(Rest),  {string_chars(Id,[First|Rest])}.
tupletor(tupletor(Id))  -->  special(First),!,  rest(Rest),  {string_chars(Id,[First|Rest])}.

lower(C)    -->  C,  char_type(C,prolog_atom_start),  !.
upper(C)    -->  C,  char_type(C,prolog_var_start),   !.
special(C)  -->  C,  C='#',                           !.

/*************
 * Utilities *
 *************/

sequence(L)          -->  L=[]                     ;  L=[E|Es],[E],   sequence(Es).          
prefix(Prefix,Text)  :-   append(Prefix,_,Text),!  .                                         
occurs(Search,Text)  :-   prefix(Search,Text),!    ;  Text=[_,Rest],  occurs(Search,Rest),!  .

str_type([], _).
str_type([First | Rest], Type) :-
	char_type(First, Type),
	str_type(Rest, Type).

% Skips 
 

/************
 * Literals *
 ************/

number(number(Number)) --> sequence(Characters), {string_chars(String,Characters),number_string(Number, String)}.

string(string(String)) --> ['"'], sequence(Contents), ['"'],
	{
		str_type(Contents, ascii),
		\+ occurs(['"'], Contents),
		\+ occurs(['\n'], Contents),
		string_chars(String, Contents)
	}.


