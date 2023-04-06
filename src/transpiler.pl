:- module(transpiler, [
	transpile_to_prolog/2
]).
:- use_module(parser).

Process(P) :-
  W -> Parse -> Tranpile -> Save -> Consult.

/*************
 * Utilities *
 *************/
cat(X,Y,Z):- string_concat(X,Y,Z).

ToString(N) :- { number_string(N, S)} , S.

Cat(X,Y) :- 
  {
    cat(X,Y,Z)
  },
  Z.

AsAtom(V) :- V -> Prepend "atom"-> Append "bar".
AsTemp(N) :- N -> ToString -> Prepend "TEMP". 
Wrap(V) :- V -> Prepend "("-> Append ")".

Foo(X) :- AsAtom@ AsTemp@ X.

SeparateBy(_,[], _) :- "".
SeparateBy(_,[S], _) :- S.
SeparateBy(D,[S | Ss]) :- 
  S -> Append Delimiter,
    -> Append SeparateBy(SS,D).

/***********
 * Program *
 ***********/

Cons(X,L) :- { O = [X|L] }, O. 

ExtractNames([]).
ExtractNames([definition(function_id(Name), _, _) | Ds], Out) :-
  ExtractNames Ds -> Prepend Name.

transpile_program(program(Definitions), Out) :-
    {
      Names = ExtractNames(Definitions),
      (Definitions_Out, Apply_Out) 
        = Transpile(Definitions, Names),
    },
    Cat(
      (Definitions_Out 
        -> SeparateBy "% Def \n" 
        -> Prepend "% Automatically Generated Definitions \n"
        -> Append "% END OF ======\n\n"
      ),
      (Apply_Out 
        -> SeparateBy "\n" 
        -> Prepend "% Automatically Generated Definitions \n"
        -> Append "======\n\n"
      )
    ).


/***************
 * Definitions *
 ***************/

transpile_definitions([], _, [], []).
transpile_definitions([D | Ds], Function_Names, [Out1 | Out2], [Apply_Out1 | Apply_Out2]) :-
	transpile_definition(D, Function_Names, Out1, Apply_Out1),
	transpile_definitions(Ds, Function_Names, Out2, Apply_Out2).

transpile_variable(function_id(I), I).

transpile_variables([V], Out) :-
	transpile_variable(V, Out).
transpile_variables([V | Vs], Out) :-
	transpile_variable(V, Out1),
	Cat(Out1, ",", Out2),
	transpile_variables(Vs, Out3),
	Cat(Out2, Out3, Out).

transpile_definition(definition(function_id(Name), Inputs, Body), Function_Names, Out, Apply_Out) :-
	%
	transpile_variables(Inputs, Out1),
	Cat(Out1, ",(", Out2),
	transpile_body(Body, Function_Names, Body_Out, Return_Vars),
	SeparateBy(Return_Vars, ",", Out3),
	Cat(Out2, Out3, Out4),
	Cat(Out4, "))", Out5),
	(Body_Out = "." ->
		Out6 = Out5
		;
		Cat(Out5, ":-", Out6)
	),
	Cat(Out6, Body_Out, Out7),
	%
	function_name(Name, Function_Name),
	Cat(Function_Name, "(", Out8),
	Cat(Out8, Out7, Out),
	%
	Cat("apply_fun(", Function_Name, Out9),
	Cat(Out9, ",", Out10),
	Cat(Out10, Out5, Out11),
	Cat(Out11, ":-", Out12),
	Cat(Out12, Out8, Out13),
	Cat(Out13, Out5, Out14),
	Cat(Out14, ".", Apply_Out).

transpile_body(Statements, Function_Names, Out, Return_Vars) :-
	transpile_statements(Statements, Function_Names, 1, Out1, Return_Vars),
	SeparateBy(Out1, ",", Out2),
	Cat(Out2, ".", Out).

/**************
 * Statements *
 **************/

transpile_statements([], _, _, [], []).
transpile_statements([S | Ss], Function_Names, Temp_Counter, Out, Return_Vars) :-
	transpile_statement(S, Function_Names, Temp_Counter, Out1, Temp_Counter1, Return_Vars1),
	transpile_statements(Ss, Function_Names, Temp_Counter1, Out2, Return_Vars2),
	append(Out1, Out2, Out),
	append(Return_Vars1, Return_Vars2, Return_Vars).

transpile_function_id_or_variable(I, Function_Names, I) :-
	\+ member(I, Function_Names), !.
transpile_function_id_or_variable(I, Function_Names, Out) :-
	member(I, Function_Names), !,
	function_name(I, Out).

transpile_statement(return(Expression), Function_Names, Temp_Counter, Statements_Out, Temp_Counter_Out, [Expression_Out]) :-
	transpile_expression(Expression, Function_Names, Temp_Counter, Statements_Out, Expression_Out, Temp_Counter_Out).

transpile_statement(imperative([]), _, Temp_Counter, [], Temp_Counter, []).
transpile_statement(imperative([S | Ss]), Function_Names, Temp_Counter, Out, Temp_Counter_Out, []) :-
	transpile_imerative_statement(S, Function_Names, Temp_Counter, Out1, Temp_Counter1),
	transpile_statement(imperative(Ss), Function_Names, Temp_Counter1, Out2, Temp_Counter_Out, _),
	append(Out1, Out2, Out).
transpile_imerative_statement(prolog_id(I), _, Temp_Counter, [I], Temp_Counter).
transpile_imerative_statement(assignment(Vs, X), Function_Names, Temp_Counter, Out, Temp_Counter_Out) :-
	transpile_variables(Vs, Out1),
	Cat("(", Out1, Out2),
	Cat(Out2, ")=", Out3),
	transpile_expression(X, Function_Names, Temp_Counter, Statements_Out, Expression_Out, Temp_Counter_Out),
	Cat(Out3, Expression_Out, Out4),
	append(Statements_Out, [Out4], Out).
transpile_imerative_statement(invocation(prolog_id(I), Arguments), Function_Names, Temp_Counter, Out, Temp_Counter_Out) :-
	Cat(I, "(", Out1),
	transpile_expressions(Arguments, Function_Names, Temp_Counter, Statements_Out, Arguments_Out, Temp_Counter_Out),
	SeparateBy(Arguments_Out, ",", Out2),
	Cat(Out1, Out2, Out3),
	Cat(Out3, ")", Out4),
	append(Statements_Out, [Out4], Out).
transpile_imerative_statement(invocation(function_id(I), Arguments), Function_Names, Temp_Counter, Out, Temp_Counter_Out) :-
	transpile_function_id_or_variable(I, Function_Names, Out1),
	Cat("apply_fun(", Out1, Out2),
	Cat(Out2, ",", Out3),
	transpile_expressions(Arguments, Function_Names, Temp_Counter, Statements_Out, Arguments_Out, Temp_Counter_Out),
	SeparateBy(Arguments_Out, ",", Out4),
	Cat(Out3, Out4, Out5),
	Cat(Out5, ",_)", Out6),
	append(Statements_Out, [Out6], Out).

/***************
 * Expressions *
 ***************/

transpile_expressions([], _, Temp_Counter, [], [], Temp_Counter).
transpile_expressions([X | Xs], Function_Names, Temp_Counter, Statements_Out, [X_Out | Xs_Out], Temp_Counter_Out) :-
	transpile_expression(X, Function_Names, Temp_Counter, Statements_Out1, X_Out, Temp_Counter1),
	transpile_expressions(Xs, Function_Names, Temp_Counter1, Statements_Out2, Xs_Out, Temp_Counter_Out),
	append(Statements_Out1, Statements_Out2, Statements_Out).

/**********************
 * Atomic Expressions *
 **********************/

transpile_expression(number(N), _, Temp_Counter, [], N_String, Temp_Counter) :-
	ToString(N, N_String).
transpile_expression(string(S), _, Temp_Counter, [], Out, Temp_Counter) :-
	Cat("\"", S, Out1),
	Cat(Out1, "\"", Out).
transpile_expression(prolog_id(I), _, Temp_Counter, [], I, Temp_Counter).
transpile_expression(function_id(I), Function_Names, Temp_Counter, [], Out, Temp_Counter) :-
	transpile_function_id_or_variable(I, Function_Names, Out).

/************************
 * Compound Expressions *
 ************************/

transpile_expression(parenthesized(X), Function_Names, Temp_Counter, Statements_Out, Out, Temp_Counter_Out) :-
	transpile_expression(X, Function_Names, Temp_Counter, Statements_Out, Out, Temp_Counter_Out).
transpile_expression(tuple(Xs), Function_Names, Temp_Counter, Statements_Out, Out, Temp_Counter_Out) :-
	transpile_expressions(Xs, Function_Names, Temp_Counter, Statements_Out, Xs_Out, Temp_Counter_Out),
	SeparateBy(Xs_Out, ",", Out1),
	Cat("(", Out1, Out2),
	Cat(Out2, ")", Out).
transpile_expression(invocation(prolog_id(I), Xs), Function_Names, Temp_Counter, Statements_Out, Out, Temp_Counter_Out) :-
	transpile_expressions(Xs, Function_Names, Temp_Counter, Statements_Out, Xs_Out, Temp_Counter_Out),
	Cat(I, "(", Out1),
	SeparateBy(Xs_Out, ",", Out2),
	Cat(Out1, Out2, Out3),
	Cat(Out3, ")", Out).
transpile_expression(invocation(function_id(I), Xs), Function_Names, Temp_Counter, Statements_Out, Temp_Name, Temp_Counter_Out) :-
	transpile_function_id_or_variable(I, Function_Names, Function_Name),
	Cat("apply_fun(", Function_Name, Statement1),
	Cat(Statement1, ",", Statement2),
	transpile_expressions(Xs, Function_Names, Temp_Counter, Statements1, Xs_Out, Temp_Counter1),
	SeparateBy(Xs_Out, ",", Statement3),
	Cat(Statement2, Statement3, Statement4),
	Cat(Statement4, ",", Statement5),
	temp_name(Temp_Counter1, Temp_Name),
	Temp_Counter_Out is Temp_Counter1 + 1,
	Cat(Statement5, Temp_Name, Statement6),
	Cat(Statement6, ")", Statement),
	append(Statements1, [Statement], Statements_Out).
