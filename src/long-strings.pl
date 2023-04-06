:- begin_tests(long_atoms).

test("long atom without newlines",true(TXT == 'this is a long atom that is written over several lines of source code')) :-
    TXT = 'this is a long atom \c
                    that is written over \c
               several lines of \c
                    source code',
    format("Written with writeq/1: ~q\n",TXT),
    format("Written with write/1:  ~w\n",TXT).

test("long atom with newlines",true(TXT == 'this is a long atom\nthat actually involves newlines and\nthe famous longcat')) :-
    TXT = 'this is a long atom\n\c
           that actually involves \c
           newlines and\n\c
           the famous longcat',
    format("Written with writeq/1: ~q\n",TXT),
    format("Written with write/1:  ~w\n",TXT).

test("data atom is 9*32 characters exactly") :-
   TXT = '5765206172652070726f756420746f20\c
          616e6e6f756e63652074686174206120\c
          68756d616e206578706c6f726174696f\c
          6e20636f6d70616e792066726f6d2053\c
          747261756d6c69205265616c6d206861',
   Length is 5*32,
   format("Written with writeq/1: ~q\n",TXT),
   atom_length(TXT,Length).

% Another way of getting multiline strings.
% atomic_list_concat/2 is not ISO though, but neither is \c

test("data atom is 9*32 characters exactly, with atomic_list_concat/2") :-
   atomic_list_concat(
      ['5765206172652070726f756420746f20',
       '616e6e6f756e63652074686174206120',
       '68756d616e206578706c6f726174696f',
       '6e20636f6d70616e792066726f6d2053',
       '747261756d6c69205265616c6d206861'],TXT),
   Length is 5*32,
   format("Written with writeq/1: ~q\n",TXT),
   atom_length(TXT,Length).

:- end_tests(long_atoms).
