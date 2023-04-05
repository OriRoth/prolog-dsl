:- begin_tests(transpiler).
:- use_module(transpiler).

compile_prolog(Prolog_Program) :-
	string_concat(":- multifile apply_fun/2.\n:- multifile apply_fun/3.\n:- multifile apply_fun/4.\n", Prolog_Program, Prolog_Program1),
	tmp_file_stream(text, File, Stream),
	write(Stream, Prolog_Program1),
	close(Stream),
	consult(File).

test(simple_program) :-
	transpile_to_prolog("Id(X) :- X.", Prolog_Program),
	compile_prolog(Prolog_Program),
	fun_Id(1, 1),
	\+ fun_Id(1, 2).

test(peano) :-
	transpile_to_prolog("Inc(N) :- s(N). Plus(N1, N2) :- {N1 = 0}, N2. Plus(N1, N2) :- { N1 = s(N) }, Plus(N, s(N2)).", Prolog_Program),
	compile_prolog(Prolog_Program),
	N0 = 0,
	fun_Inc(N0, N1),
	fun_Plus(N1, N1, N2),
	fun_Plus(N2, N2, s(s(s(s(0))))).

test(church_numerals) :-
	transpile_to_prolog("N0(_, X) :- X. Inc(N, F, X) :- F(N(F, X)). Plus(N, M, F, X) :- M(F, N(F, X)).", Prolog_Program),
	compile_prolog(Prolog_Program).

test(church_booleans) :-
	transpile_to_prolog("T(X, _) :- X. F(_, Y) :- Y. Not(A) :- A(F, T). Or(A, B) :- A(T, B). And(A, B) :- A(B, F).", Prolog_Program),
	compile_prolog(Prolog_Program),
	fun_T(1, 2, 1),
	fun_F(1, 2, 2),
	fun_Not(fun_T, fun_F),
	fun_And(fun_T, fun_T, fun_T),
	fun_And(fun_T, fun_F, fun_F),
	fun_Or(fun_T, fun_F, fun_T),
	fun_Or(fun_F, fun_F, fun_F),
	!.

:- end_tests(transpiler).
