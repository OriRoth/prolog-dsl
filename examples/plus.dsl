#true _ _ _ :- #1.
#false _ _ _ :- #2.
#error _ _ _ :- #3.
#not _ :- #1 ? #false : #true ^ #error.
#and _ _ :- #1 ? #2 : #false ^ #error.
#or _ _ :- #1 ? #true : #2 ^ #error.
#xor _ _ :- #1 ? #not #2 : #2 ^ #error.
#xor3 _ _ _ :- #xor (#xor #1 #2) #3.
#bot :- #error, #error.
#n0 :- #false, #bot.
#n1 :- #true, #bot.
#plus _ _ :- #plusCarry #1 #2 #false
#plusCarry (_, _) (_, _) _ :-
	{ HasCarry := #or (#or (#and #1 #3) (#and #1 #5)) (#and #3 #5) },
	#1 ^ (#3, #4) ?:
	#3 ^ (#1, #2) ?:
	(
		#xor3 #1 #3 #5,
		#plusCarry #2 #4 (HasCarry ? #true : #false)
	).
