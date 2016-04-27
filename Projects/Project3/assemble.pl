 % assemble

assemble(Asm, Words) :-
  phrase(assemble(Asm, 0, [], 0, 0), Words).

assemble([], Acc, Const, LocalCnt, _ProgramCnt) -->
  { LC is LocalCnt + 1, pad(Acc, LC, NewAcc) },
  [ NewAcc ], Const.

assemble([H|T], Acc, Const, LocalCnt, ProgramCnt) -->
  ( { H = nop,      !, NewAcc is Acc,       LC is LocalCnt + 1, NewConst = Const }
  ; { H = syscall,  !, NewAcc is Acc + 1,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = load,     !, NewAcc is Acc + 2,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = store,    !, NewAcc is Acc + 3,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = swapa,    !, NewAcc is Acc + 4,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = swapd,    !, NewAcc is Acc + 5,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = branchz,  !, NewAcc is Acc + 6,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = branchn,  !, NewAcc is Acc + 7,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = jump,     !, TmpAcc is Acc + 8,
      TC is LocalCnt + 1,
      pad(TmpAcc, TC, NewAcc),            LC is 4, NewConst = Const
    }
  ; { H = const(V), !, NewAcc is Acc + 9,
      append(Const, [V], NewConst),       LC is LocalCnt + 1 }
  ; { H = add,      !, NewAcc is Acc + 10,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = sub,      !, NewAcc is Acc + 11,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = mul,      !, NewAcc is Acc + 12,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = div,      !, NewAcc is Acc + 13,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = shift,    !, NewAcc is Acc + 14,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = nand,     !, NewAcc is Acc + 15,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = label(Addr), !,
      ( LocalCnt = 0, !,
        Addr = ProgramCnt,
        LC is LocalCnt,
        NewAcc is Acc
      ; TC is LocalCnt + 1,
        pad(Acc, TC, NewAcc),
        LC is 4,
        Addr = PC
      ), NewConst = Const
    }
  ),
  ( { LC = 4 }, !,
    [ NewAcc ], NewConst,
    { RC is 0, length(Const, CC),
      PC is ProgramCnt + CC + 1,
      A is 0,
      C = []
    }
  ; { RC is LC,
      PC is ProgramCnt,
      A is NewAcc * 16,
      C = NewConst
    }
  ), assemble(T, A, C, RC, PC).
pad(Acc, 4, Acc).
pad(Acc, Lc, Ret) :-
  NewAcc is 16 * Acc,
  NewLc is Lc + 1,
  pad(NewAcc, NewLc, Ret).

compile(Text) :-
  resolve(Text, V),
  phrase(generateProgram(V), Asm),
  phrase(assemble(Asm, 0, [], 0, 0), B),
  printHex(B).

printHex([]).
printHex([H|T]) :-
  format('~|~`0t~16r~4+~n', H),
  printHex(T).
