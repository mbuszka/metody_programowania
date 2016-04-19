%  lexer

lexer(Tokens) -->
  whitespace,
  ( ":=",       !, { Token = tAssgn }
  ; ";",        !, { Token = tSCol }
  ; ",",        !, { Token = tComma }
  ; "+",        !, { Token = tPlus }
  ; "-",        !, { Token = tMinus }
  ; "*",        !, { Token = tTimes }
  ; "(",        !, { Token = tLPar }
  ; ")",        !, { Token = tRPar }
  ; "=",        !, { Token = tEq }
  ; "<>",       !, { Token = tNeq }
  ; "<=",       !, { Token = tLeq }
  ; "<",        !, { Token = tLt }
  ; ">=",       !, { Token = tGeq }
  ; ">",        !, { Token = tGt }
  ; digit(D),   !,
      number(D, N),
      { Token = tNum(N) }
  ; letter(L),  !,
      identifier(L, Id),
      { isKeyword(Id, Token), !
      ; Token = tIdent(Id)
      }
  ; [_],
      { Token = tUnknown }
  ),
  !,
  { Tokens = [Token| TList] },
  lexer(TList).
lexer([]) -->
  [].

commentEnd -->
  ( "*)", !
  ; [_], commentEnd
  ).

whitespace -->
  ( "(*", !, commentEnd, whitespace
  ; [Char], { code_type(Char, space) }, !, whitespace
  ; []
  ).

digit(D) -->
  [D],
  { code_type(D, digit)}.

digits([D|T]) -->
  digit(D),
  !,
  digits(T).
digits([]) -->
  [].

number(D, N) -->
  digits(DS),
  { number_chars(N, [D| DS]) }.

letter(L) -->
  [L],
  { code_type(L, alpha) }.

identStr([L| T]) -->
  [L],
  { code_type(L, alnum) ; char_code('_', L); char_code('''', L) },
  !,
  identStr(T).
identStr([]) -->
  [].

identifier(L, Id) -->
  identStr(LS),
  { atom_codes(Id, [L| LS])}.

isKeyword(Id, Token) :-
  member(Id-Token, [ and-tAnd,
                     begin-tBegin,
                     call-tCall,
                     div-tDiv,
                     do-tDo,
                     done-tDone,
                     else-tElse,
                     end-tEnd,
                     fi-tFi,
                     if-tIf,
                     local-tLocal,
                     mod-tMod,
                     not-tNot,
                     or-tOr,
                     procedure-tProcedure,
                     program-tProgram,
                     read-tRead,
                     return-tReturn,
                     then-tThen,
                     value-tValue,
                     while-tWhile,
                     write-tWrite ]).
