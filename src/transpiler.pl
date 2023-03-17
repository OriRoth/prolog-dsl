:- module(transpiler, [
	transpile_to_prolog/2
]).
:- use_module(parser).

/*************
 * Utilities *
 *************/

join([], _, "").
join([String], _, String).
join([String | Strings], Delimiter, Out) :-
	string_concat(String, Delimiter, Out1),
	join(Strings, Delimiter, Out2),
	string_concat(Out1, Out2, Out).

function_name(Function_Id, Out) :-
	string_concat("fun_", Function_Id, Out).

temp_name(Temp_Number, Temp_Name) :-
	number_string(Temp_Number, Temp_Number_String),
	string_concat("TEMP", Temp_Number_String, Temp_Name).

/***********
 * Program *
 ***********/

get_function_names([], []).
get_function_names([definition(function_id(Name), _, _) | Ds], Out) :-
	get_function_names(Ds, Out1),
	append([Name], Out1, Out).

transpile_program(program(Definitions), Out) :-
	get_function_names(Definitions, Function_Names),
	transpile_definitions(Definitions, Function_Names, Definitions_Out),
	join(Definitions_Out, "\n", Out).

transpile_to_prolog(W, Out) :-
	parse(W, AST),
	transpile_program(AST, Out),
	!.

/***************
 * Definitions *
 ***************/

transpile_definitions([], _, []).
transpile_definitions([D | Ds], Function_Names, [Out1 | Out2]) :-
	transpile_definition(D, Function_Names, Out1),
	transpile_definitions(Ds, Function_Names, Out2).

transpile_variable(function_id(I), I).

transpile_variables([V], Out) :-
	transpile_variable(V, Out).
transpile_variables([V | Vs], Out) :-
	transpile_variable(V, Out1),
	string_concat(Out1, ",", Out2),
	transpile_variables(Vs, Out3),
	string_concat(Out2, Out3, Out).

transpile_definition(definition(function_id(Name), Inputs, Body), Function_Names, Out) :-
	function_name(Name, Function_Name),
	string_concat("apply_fun(", Function_Name, Out1),
	string_concat(Out1, ",", Out2),
	transpile_variables(Inputs, Out3),
	string_concat(Out2, Out3, Out4),
	string_concat(Out4, ",(", Out5),
	transpile_body(Body, Function_Names, Body_Out, Return_Vars),
	join(Return_Vars, ",", Out6),
	string_concat(Out5, Out6, Out7),
	string_concat(Out7, "))", Out8),
	(Body_Out = "." ->
		Out9 = Out8
		;
		string_concat(Out8, ":-", Out9)
	),
	string_concat(Out9, Body_Out, Out).

transpile_body(Statements, Function_Names, Out, Return_Vars) :-
	is_list(Statements), !,
	transpile_statements(Statements, Function_Names, 1, Out1, Return_Vars),
	join(Out1, ",", Out2),
	string_concat(Out2, ".", Out).

transpile_body(Expression, Function_Names, Out, Return_Vars) :-
	\+ is_list(Expression), !,
	transpile_statement(return(Expression), Function_Names, 1, Out1, _, Return_Vars),
	join(Out1, ",", Out2),
	string_concat(Out2, ".", Out).

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

transpile_statement(imperative(prolog_id(I)), _, Temp_Counter, [I], Temp_Counter, []).
transpile_statement(imperative(assignment(Vs, X)), Function_Names, Temp_Counter, Out, Temp_Counter_Out, []) :-
	transpile_variables(Vs, Out1),
	string_concat("(", Out1, Out2),
	string_concat(Out2, ")=", Out3),
	transpile_expression(X, Function_Names, Temp_Counter, Statements_Out, Expression_Out, Temp_Counter_Out),
	string_concat(Out3, Expression_Out, Out4),
	append(Statements_Out, [Out4], Out).
transpile_statement(imperative(invocation(prolog_id(I), Arguments)), Function_Names, Temp_Counter, Out, Temp_Counter_Out, []) :-
	string_concat(I, "(", Out1),
	transpile_expressions(Arguments, Function_Names, Temp_Counter, Statements_Out, Arguments_Out, Temp_Counter_Out),
	join(Arguments_Out, ",", Out2),
	string_concat(Out1, Out2, Out3),
	string_concat(Out3, ")", Out4),
	append(Statements_Out, [Out4], Out).
transpile_statement(imperative(invocation(function_id(I), Arguments)), Function_Names, Temp_Counter, Out, Temp_Counter_Out, []) :-
	transpile_function_id_or_variable(I, Function_Names, Out1),
	string_concat("apply_fun(", Out1, Out2),
	string_concat(Out2, ",", Out3),
	transpile_expressions(Arguments, Function_Names, Temp_Counter, Statements_Out, Arguments_Out, Temp_Counter_Out),
	join(Arguments_Out, ",", Out4),
	string_concat(Out3, Out4, Out5),
	string_concat(Out5, ",_)", Out6),
	append(Statements_Out, [Out6], Out).

transpile_statement(return(Expression), Function_Names, Temp_Counter, Statements_Out, Temp_Counter_Out, [Expression_Out]) :-
	transpile_expression(Expression, Function_Names, Temp_Counter, Statements_Out, Expression_Out, Temp_Counter_Out).

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
	number_string(N, N_String).
transpile_expression(string(S), _, Temp_Counter, [], Out, Temp_Counter) :-
	string_concat("\"", S, Out1),
	string_concat(Out1, "\"", Out).
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
	join(Xs_Out, ",", Out1),
	string_concat("(", Out1, Out2),
	string_concat(Out2, ")", Out).
transpile_expression(invocation(prolog_id(I), Xs), Function_Names, Temp_Counter, Statements_Out, Out, Temp_Counter_Out) :-
	transpile_expressions(Xs, Function_Names, Temp_Counter, Statements_Out, Xs_Out, Temp_Counter_Out),
	string_concat(I, "(", Out1),
	join(Xs_Out, ",", Out2),
	string_concat(Out1, Out2, Out3),
	string_concat(Out3, ")", Out).
transpile_expression(invocation(function_id(I), Xs), Function_Names, Temp_Counter, Statements_Out, Temp_Name, Temp_Counter_Out) :-
	transpile_function_id_or_variable(I, Function_Names, Function_Name),
	string_concat("apply_fun(", Function_Name, Statement1),
	string_concat(Statement1, ",", Statement2),
	transpile_expressions(Xs, Function_Names, Temp_Counter, Statements1, Xs_Out, Temp_Counter1),
	join(Xs_Out, ",", Statement3),
	string_concat(Statement2, Statement3, Statement4),
	string_concat(Statement4, ",", Statement5),
	temp_name(Temp_Counter1, Temp_Name),
	Temp_Counter_Out is Temp_Counter1 + 1,
	string_concat(Statement5, Temp_Name, Statement6),
	string_concat(Statement6, ")", Statement),
	append(Statements1, [Statement], Statements_Out).
