True (A, _, _) = A
False (_, A, _) = A
Error (_, _, A) = A
IsError(T) = T(False, False, True)
Not = #(False, True, Error)
Or (B1, B2) = B1(True, B2, Error) /* invoke(B1, (fun_True, B2, fun_Error)) */
And (B1, B2) = B1(B2, False, Error)
Xor(T1, T2) = Or(And(T1, Not(T2)), And(Not(T1), T2))
If(C, T, F, E) = C(T, F, E)
/* TODO add Copy */

Inc = s(#)
Plus (N1, N2) = (
	* N1 = 0,
	N2
)
Plus (N1, N2) = (
	* N1 = s(N),
	* write(N),
	Inc(N),
	write(N1),
	Plus(N, s(N2))
)

Cons(A, B) = (A, B)
Car(C) = (
	* (A, _) <- C,
	A
)
Cdr(C) = (
	* (_, B) <- C,
	B
)

Nbot = (Error, Error)
N0 = (False, Nbot)
N1 = (True, Nbot)

Add2(T1, T2) = (Xor(T1, T2), And(T1, T2))
Add3(T1, T2, T3) = (Xor(Xor(T1, T2), T3), Or(And(T1, Or(T2, T3)), And(T2, T3)))

AddList(L1, L2) = (
	* (Bit, Carry) <- Add2(Car(L1), Car(L2)),
	Cons(Bit, AddListCarry(Cdr(L1), Cdr(L2), Carry))
)
AddListCarry(L1, L2, Carry) = (
	* T1 <- Car(L1),
	* T2 <- Car(L2),
	* (Bit, NewCarry) <- Add3(T1, T2, Carry),
	* A <- AddListCarry(Cdr(L1), Cdr(L2), NewCarry),
	* I <- If(IsError(T2), L1, Cons(Bit, A), Error),
	If(IsError(T1), L2, I, Error)
)
