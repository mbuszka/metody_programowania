 % resolve call by name actual arguments into thunks, which are added into
 % callers environment
 % by default they will return calculated value, if
 % a reference to variable is passed one can write into it,
 % otherwise behaviour is undefined
 % evaluating thunkCall will push result of calculation and its address,
 % so if thunk is only a reference it will properly point to a variable

resolveThunksProcCall(procCall(Addr, Params), Thunks, procCall(Addr, ParamsT)) :-
  resolveThunksParams(Params, Thunks, ParamsT).

resolveThunksParams([], [], []).
resolveThunksParams([H|T], Thunks, [HR|TR]) :-
  ( H = name(thunkCall(Addr)), !,
    HR = thunkPass(Addr),
    Thunks = Rest
  ; H = name(variable(Addr)), !,
    HR = referencePass(Addr),
    Thunks = [proc(Addr, [], [], [return(variable(2))]) | Rest]
  ; H = name(Exp), !,
    HR = thunkPass(Addr),
    Thunks = [proc(Addr, [], [], [return(Exp)]) | Rest]
  ; HR = H,
    Thunks = Rest
  ), resolveThunksParams(T, Rest, TR).

resolveThunksProc(proc(Addr, Params, Block), proc(Addr, Params, Decl, Ins)) :-
  resolveThunksBlock(Block, b(Decl, Ins)).

resolveThunksBlock(b(Ds, Ins), b(Dst, Inst)) :-
  resolveThunksDeclarations(Ds, Dsr),
  resolveThunksInstructions(Ins, Thunks, Inst),
  append(Dsr, Thunks, Dst).

resolveThunksDeclarations([], []).
resolveThunksDeclarations([H|T], [HR|TR]) :-
  ( H = proc(_, _, _), !,
      resolveThunksProc(H, HR)
  ; HR = H
  ), resolveThunksDeclarations(T, TR).

resolveThunksInstructions([], [], []).
resolveThunksInstructions([H|T], Thunks, [HR| TR]) :-
  resolveThunksInstruction(H, ThunksH, HR),
  append(ThunksH, ThunksT, Thunks),
  resolveThunksInstructions(T, ThunksT, TR).

resolveThunksInstruction(I, Thunks, Iv) :-
  ( I = if(Bool, Then, Else), !,
      resolveThunksBool(Bool, ThunksB, Boolv),
      resolveThunksInstructions(Then, ThunksT, Thenv),
      resolveThunksInstructions(Else, ThunksE, Elsev),
      append([ThunksB, ThunksT, ThunksE], Thunks),
      Iv = if(Boolv, Thenv, Elsev)
  ; I = if(Bool, Then), !,
      resolveThunksBool(Bool, ThunksB, Boolv),
      resolveThunksInstructions(Then, ThunksI, Thenv),
      append(ThunksB, ThunksI, Thunks),
      Iv = if(Boolv, Thenv)
  ; I = while(Bool, Ins), !,
      resolveThunksBool(Bool, ThunksB, Boolv),
      resolveThunksInstructions(Ins, ThunksI, Insv),
      append(ThunksB, ThunksI, Thunks),
      Iv = while(Boolv, Insv)
  ; I = discardReturn(Call), !,
      resolveThunksProcCall(Call, Thunks, Callv),
      Iv = discardReturn(Callv)
  ; I = return(Exp), !,
      resolveThunksExpr(Exp, Thunks, Expv),
      Iv = return(Expv)
  ; I = ioRead(V), !,
      resolveThunksVar(V, Thunks, Vv),
      Iv = ioRead(Vv)
  ; I = ioWrite(Exp), !,
      resolveThunksExpr(Exp, Thunks, Expv),
      Iv = ioWrite(Expv)
  ; I = assgn(V, Exp),
      resolveThunksExpr(Exp, Thunks, Expv),
      ( V = thunkCall(Addr), !,
        Vr = thunkWrite(Addr)
      ; Vr = V
      ),
      Iv = assgn(Vr, Expv)
  ).

resolveThunksExpr(Expr, Thunks, Exprv) :-
  ( Expr = const(_), !,
      Thunks = [],
      Exprv = Expr
  ; Expr = variable(_Addr), !,
      Thunks = [],
      Exprv = Expr
  ; Expr = thunkCall(_Addr), !,
      Thunks = [],
      Exprv = Expr
  ; Expr = procCall(_Addr, _Params), !,
      resolveThunksProcCall(Expr, Thunks, Exprv)
  ; Expr = neg(E), !,
      resolveThunksExpr(E, Thunks, Ev),
      Exprv = neg(Ev)
  ; Expr =.. [Op, Left, Right], !,
      resolveThunksExpr(Left, ThunksL, Leftv),
      resolveThunksExpr(Right, ThunksR, Rightv),
      append(ThunksL, ThunksR, Thunks),
      Exprv =.. [Op, Leftv, Rightv]
  ).

resolveThunksBool(Bool, Thunks, Boolv) :-
  ( Bool = not(B), !,
      resolveThunksBool(B, Thunks, Bv),
      Boolv = not(Bv)
  ; Bool = and(Left, Right), !,
      resolveThunksBool(Left, ThunksL, Leftv),
      resolveThunksBool(Right, ThunksR, Rightv),
      append(ThunksL, ThunksR, Thunks),
      Boolv = and(Leftv, Rightv)
  ; Bool = or(Left, Right), !,
      resolveThunksBool(Left, ThunksL, Leftv),
      resolveThunksBool(Right, ThunksR, Rightv),
      append(ThunksL, ThunksR, Thunks),
      Boolv = or(Leftv, Rightv)
  ; Bool =.. [Op, Left, Right], !,
      resolveThunksExpr(Left, ThunksL, Leftv),
      resolveThunksExpr(Right, ThunksR, Rightv),
      append(ThunksL, ThunksR, Thunks),
      Boolv =.. [Op, Leftv, Rightv]
  ).
