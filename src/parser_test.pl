:- begin_tests(parser).
:- use_module(lexer).
:- use_module(parser).

parse_atomic_expression(W, AST) :-
	tokenize(W, Tokens),
	phrase(atomic_expression(AST), Tokens).
test(atomic_expression) :-
	parse_atomic_expression("0", number(0)),
	parse_atomic_expression("0.1", number(0.1)),
	parse_atomic_expression("\"a\"", string("a")),
	parse_atomic_expression("a", prolog_id("a")),
	parse_atomic_expression("A", function_id("A")),
	parse_atomic_expression("_", function_id("_")),
	!.

parse_compound_expression(W, AST) :-
	tokenize(W, Tokens),
	phrase(compound_expression(AST), Tokens).
test(compound_expression) :-
	parse_compound_expression("Foo(bar, 0)",
		invocation(function_id("Foo"), [prolog_id("bar"), number(0)])),
	parse_compound_expression("foo(bar, 0)",
		invocation(prolog_id("foo"), [prolog_id("bar"), number(0)])),
	parse_compound_expression("Foo(bar, 0, Foo(bar, 0))",
		invocation(function_id("Foo"), [prolog_id("bar"), number(0),
			invocation(function_id("Foo"), [prolog_id("bar"), number(0)])])),
	parse_compound_expression("(bar, 0)",
		tuple([prolog_id("bar"), number(0)])),
	parse_compound_expression("(((bar, 0)))",
		tuple([prolog_id("bar"), number(0)])),
	\+ parse_compound_expression("Foo", _),
	\+ parse_compound_expression("Foo()", _),
	\+ parse_compound_expression("()", _),
	\+ parse_compound_expression("(X <- Y)", _),
	!.

parse_statement(W, AST) :-
	tokenize(W, Tokens),
	phrase(statement(AST), Tokens).
test(statement) :-
	parse_statement("* X <- Y", imperative(assignment([function_id("X")], function_id("Y")))),
	parse_statement("* (X) <- Y", imperative(assignment([function_id("X")], function_id("Y")))),
	parse_statement("* (X1, X2, X3) <- Y", imperative(assignment([function_id("X1"), function_id("X2"), function_id("X3")], function_id("Y")))),
	parse_statement("* foo", imperative(prolog_id("foo"))),
	parse_statement("foo", return(prolog_id("foo"))),
	!.

parse_definition(W, AST) :-
	tokenize(W, Tokens),
	phrase(definition(AST), Tokens).
test(definition) :-
	parse_definition("Id(X) = X.", definition(
		function_id("Id"),
		[function_id("X")],
		function_id("X"))),
	parse_definition("Bar(X) = (* baz(X, Y), Y)", definition(
		function_id("Bar"),
		[function_id("X")],
		[
			imperative(invocation(prolog_id("baz"), [function_id("X"), function_id("Y")])),
			return(function_id("Y"))
		])),
	parse_definition("Foo (X1) =\n\t(* Z <- Bar(Baz(X1), 13),\n\t* faz(Y, Z), X2)", definition(
		function_id("Foo"),
		[function_id("X1")],
		[
			imperative(assignment([function_id("Z")], _)),
			imperative(invocation(prolog_id("faz"), _)),
			return(function_id("X2"))
		])),
	parse_definition("Plus (N1, N2) = (*N1 <- s(N), Plus(N, N2))", definition(
		function_id("Plus"),
		[function_id("N1"), function_id("N2")],
		[imperative(assignment(_, _)), return(invocation(_, _))])),
	!.

test(program) :-
	parse("Id(X)=X.Id(X)=X.", program([definition(_, _, _), definition(_, _, _)])),
	!.

:- end_tests(parser).
