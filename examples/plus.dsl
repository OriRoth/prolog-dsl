#true   _  _  _  :-  #1.                
#false  _  _  _  :-  #2.                
#error  _  _  _  :-  #3.                
#T      _  _  _  :-  #true   #          
#F      _  _  _  :-  #false  #.         
#E      _  _  _  :-  #error  #.         
#O      _  _  _  :-  #       =>  #true  .
#l      _  _  _  :-  #false  #1  #2     #3.

#not  _  :-  #1  ?  #F  :  #T  ^  #E.
#not  _  :-  #   ?  #F  :  #T  ^  #E.
#not  _  :-  #   ?  #F  :  #T  ^  .

#and  _  _  :-  #1  ?  #2    :   #F  ^   #E.  
#or   _  _  :-  #1  ?  #T    :   #2  ^   #E.  
#xor  _  _  :-  #1  ?  #not  #2  :   #2  ^    #E.

#xor3  _  _  _  :-  #xor  (#xor  #1  #2)  #3.        
#or3   _  _  _  :-  #or   #1  #2  =>   #or   #3.
#and3  _  _  _  :-  #and  #1  #2  =>   #and  #3.

#bot :- #E, #E.
#n0 :- #F, #bot.
#n1 :- #T, #bot.
#plus N1 N2 :- #plus_ N1 N2 #O.

#majority _ _ _ :- #or3 (#and #1 #2) (#and #1 #2) (#and #2 #3).


#plus_ N1 N2 C:-
  {
    (B1,Rest1) := N1,
    (B2,Rest2) := N2,
    HasC := #majority #B1 #B2 C, 
    B := #xor3 B1 B2 C,
    #rest :- #plus_ Rest1 Rest2 (HasC ? #T : #F)
  }
  B1 ^ N2 ?: B2 ^ N1 ?: (B,#rest).
                T   F E
  (a > b ? a) | a  | F | E 
  : b         | a  | b | E

  if a>b is false, it returns b.

  ^
  :
  ?
  ^?
  ^:
  :^
  :?
  ?^
  ?:


