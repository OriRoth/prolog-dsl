fun T x _ _ = x
fun F _ x _ = x
fun E _ _ x = x
              
fun NOT b = b F T E
fun PAIR x y z w = w x y z
fun COPY x = x (PAIR T T T) (PAIR F F F) (PAIR E E E)

fun AND a b = a b F E
fun OR a b = a T b E
fun XOR a b = a (NOT b) b E

fun AND3 a b c = AND (AND a b) c
fun OR3 a b c = OR (OR a b) c
fun XOR3 a b c = XOR (XOR a b) c

fun majority a b c =
	(COPY b) (fn b1 => fn b2 => fn b3 =>
	(COPY c) (fn c1 => fn c2 => fn c3 =>
		a (b1 T c1 E) (b2 c2 F E) E
	)) 

  (*
val BOT = (E, E)
val O = (F, BOT)
val l = (T, BOT)
fun plus_ F N1 N2 C = let
		val (B1, Rest1) = N1
		val (B2, Rest2) = N2
		val HasC = OR3 (AND B1 B2) (AND B1 C) (AND B2 C)
		val B = XOR3 B1 B2 C
		val REST = F F Rest1 Rest2 HasC
		val $ = (B, REST)
	in
		(OR3 B1 B2 T) $ E E
	end
fun plus N1 N2 = (plus_ plus_) N1 N2 F


fun plus_ 
      (n1:('A*'B)) 
      (n2:('C*'D)) 
      (b3:'E) 
      (majority:('M1*'M2*'M3)->'M4) 
      (xor3:('X1*'X2*'X3)->'X4) 
  = let
      val (b1, Rest1) = n1
      val (b2, Rest2) = n2
      val c = majority(b1,b2,b3)  
      val b = xor3(b1,b2,b3)
      val REST = plus_ Rest1 Rest2 c majority xor3
      val $ = (b, REST)
    in
      (OR3 b1 b2 T) $ E E
    end

fun plus N1 N2 = plus_ N1 N2 F majority XOR3
  *)
