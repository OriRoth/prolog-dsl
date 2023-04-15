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
tuplers(Tuplers)        -->  '',  Tuplers=[];     tupler(T),tupler(Ts),             Tuplers=[T|Ts].
parameters(Parameters)  -->  '',  Parameters=[];  parameter(P),',',parameters(Ps),  Parameters=[P|Ps].
components(Components)  -->  '',  Components=[];  component(C),',',components(Cs),  Components=[C|Cs].

% A list of tuplers comprises a program. A tupler is a triple compirsed by
% name, parameters, and body. The body is a tuple of zero or more expressions
% to be computed and retuned as a tuple when the tupler is invoked on specific
% values of the parameters. 
program(Tuplers)                -->  tuplers(Tuplers).                      
tupler((Name,Parameters,Body))  -->  header(Name,Parameters),               body(Body).
header(Name,Pattern)            -->  tuple_id(Name),                        parameters(Parameters).
body(Return)                    -->  ':-',components(Cs),'.'                assemble(Cs,Tuple).

% A tuple is a pair of constituents and auxiliaries. Both elements two possibly
% empty lists, the elements and the body. A tuple is nullaryItems of both lists
% may be either components or other tuples. 
%
% The first list is of the tuple <em>elements</em>. The other list is of
% <em>hidden</em> tuples, which are not part of the values comprising the
% tuple, but are computed (collaterally) as part of the computation of the
% tuple.

% Tuples come in different varieties.
unit(I)       :-  I=t(Elements,Hidden),  empty(Elements),  any(Hidden).
procedure(P)  :-  P=t(Elements,Hidden),  empty(Elements),  not_empty(Hidden).
singleton(S)  :-  S=t(Elements,Hidden),  one(Elements),    empty(Hidden).
function(F)   :-  F=t(Elements,Hidden),  one(Elements),    not_empty(Hidden).
pair(S)       :-  S=t(Elements,Hidden),  two(Elements),    empty(Hidden).

tuple(t(Elements,Hidden))   -->  tuple(Elements,Hidden).                                          
tuple(Elements,Hidden)      -->  '{',components(Cs),'}',                Elements=Cs,              Hidden=[].
tuple(Elements,Hidden)      -->  '(',components(Cs),')',                assemble(Cs,Tuple).       
assemble(Components,Tuple)  :-   classify(Components,Elements,Hidden),  Tuple=(Elements,Hidden).  


% An expression is either a tuple, or a component.
expression(t(Elements,Hidden))  -->  tuple(Elements,Hidden).
expression(c(C))                -->  component(C).

assignment(a(Pattern, X))       --> pattern(Pattern),':=',expression(X).
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

% slot (a single variable) vs slots (a possible compound set of variables) 
pattern(V)  --> variable(V).
pattern(P)  --> '(', patterns(P), ')'.                          


literal(number(N)) --> [number(N)].
literal(string(N)) --> [string(N)].
literal(prolog_id(I)) --> [prolog_id(I)].
