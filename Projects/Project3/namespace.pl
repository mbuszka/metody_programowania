% resolve indentifiers namespace

validateBlock(b(Ds, Ins), Names, b(Dsv, Insv)) :-
  validateDeclarations(Ds, Names, NewNames, Dsv),
  validateInstructions(Ins, NewNames, Insv).

validateDeclarations(Ds, Names, NewNames, Dsv) :-
  validateDeclarations(Ds, 0, Names, NewNames, Dsv).

validateDeclarations([], _, Names, Names, []).
validateDeclarations([H|T], Clocal, Names, Ret, Dsv) :-
  ( H = local(_Id, _Addr, Clocal), !,
      Cnt is Clocal + 1,
      NewNames = [H| Names],
      Dsv = [H| Tv]
  ; H = proc(Id, Addr, Args, Block), !,
      Cnt is Clocal,
      validateFormalParameters(Args), % validate so that they have different names
      length(Args, ArgsC),
      append(Args, [rec(Id, Addr, ArgsC)| Names], BlockNames), % maybe should reverse them ?
      validateBlock(Block, BlockNames, Blockv),
      NewNames = [proc(Id, Addr, ArgsC)| Names],
      Dsv = [proc(Id, Addr, Args, Blockv)| Tv]
  ), validateDeclarations(T, Cnt, NewNames, Ret, Tv).

validateFormalParameters(Args) :-
  validateFormalParameters(Args, 0, 0).

validateFormalParameters([], _, _).
validateFormalParameters([H| T], Cname, Cvalue) :-
  ( H = value(_Id, _Addr, Cvalue), !,
      CntV is Cvalue + 1,
      CntN = Cname
  ; H = name(_Id, _Addr, Cname),
      CntN is Cname + 1,
      CntV = Cvalue
  ), validateFormalParameters(T, CntN, CntV).

validateInstructions([], _, []).
validateInstructions([H|T], Names, [Hv|Tv]) :-
  validateInstruction(H, Names, Hv),
  validateInstructions(T, Names, Tv).

validateInstruction(I, Names, Iv) :-
  ( I = if(Bool, Then, Else), !,
      validateBool(Bool, Names, Boolv),
      validateInstructions(Then, Names, Thenv),
      validateInstructions(Else, Names, Elsev),
      Iv = if(Boolv, Thenv, Elsev)
  ; I = if(Bool, Then), !,
      validateBool(Bool, Names, Boolv),
      validateInstructions(Then, Names, Thenv),
      Iv = if(Boolv, Thenv)
  ; I = while(Bool, Ins), !,
      validateBool(Bool, Names, Boolv),
      validateInstructions(Ins, Names, Insv),
      Iv = while(Boolv, Insv)
  ; I = discardReturn(Call), !,
      validateProc(Call, Names, Callv),
      Iv = discardReturn(Callv)
  ; I = return(Exp), !,
      validateExpr(Exp, Names, Expv),
      Iv = return(Expv)
  ; I = ioRead(V), !,
      validateVar(V, Names, Vv),
      Iv = ioRead(Vv)
  ; I = ioWrite(Exp), !,
      validateExpr(Exp, Names, Expv),
      Iv = ioWrite(Expv)
  ; I = assgn(V, Exp),
      validateVar(V, Names, Vv),
      validateExpr(Exp, Names, Expv),
      Iv = assgn(Vv, Expv)
  ).

validateExpr(Expr, Names, Exprv) :-
  ( Expr = const(_), !,
      Exprv = Expr
  ; Expr = variable(_Id, _Addr), !,
      validateVar(Expr, Names, Exprv)
  ; Expr = procCall(_Id, _Addr, _Params), !,
      validateProc(Expr, Names, Exprv)
  ; Expr = neg(E), !,
      validateExpr(E, Names, Ev),
      Exprv = neg(Ev)
  ; Expr =.. [Op, Left, Right], !,
      validateExpr(Left, Names, Leftv),
      validateExpr(Right, Names, Rightv),
      Exprv =.. [Op, Leftv, Rightv]
  ).

validateBool(Bool, Names, Boolv) :-
  ( Bool = not(B), !,
      validateBool(B, Names, Bv),
      Boolv = not(Bv)
  ; Bool = and(Left, Right), !,
      validateBool(Left, Names, Leftv),
      validateBool(Right, Names, Rightv),
      Boolv = and(Leftv, Rightv)
  ; Bool = or(Left, Right), !,
      validateBool(Left, Names, Leftv),
      validateBool(Right, Names, Rightv),
      Boolv = or(Leftv, Rightv)
  ; Bool =.. [Op, Left, Right], !,
      validateExpr(Left, Names, Leftv),
      validateExpr(Right, Names, Rightv),
      Boolv =.. [Op, Leftv, Rightv]
  ).

validateProc(procCall(Id, Addr, Params), Names, procCall(Id, Addr, Paramsv)) :-
  length(Params, ParamsC),
  validateProcId(Id, Addr, ParamsC, Names), !,
  validateActualParams(Params, Names, Paramsv).

validateProcId(Id, Addr, ParamsC, [N| _]) :-
  ( N = proc(Id, Addr, ParamsC), !
  ; N = rec(Id, Addr, ParamsC)
  ), !.
validateProcId(Id, Addr, ParamsC, [_| Names]) :-
  validateProcId(Id, Addr, ParamsC, Names).

validateActualParams([], _, []).
validateActualParams([H| T], Names, [Hv| Tv]) :-
  validateExpr(H, Names, Hv),
  validateActualParams(T, Names, Tv).

validateVar(V, Names, Vv) :-
  validateVar(V, Names, _, Vv).
validateVar(variable(Id, Addr), [N| _Names], Scope, variable(Id, Addr)) :-
  ( N = local(Id, Addr, _), !
  ; N = name(Id, Addr, _), !
  ; N = value(Id, Addr, _)
  ), (var(Scope) -> Scope = local), !.
validateVar(V, [N| Names], Scope, Vv) :-
  ( N = rec(_, _), !, Scope = parent,
      validateVar(V, Names, Scope, Vv)
  ; validateVar(V, Names, Scope, Vv)
  ), !.
validateVar(_, [], _, _) :- fail.
