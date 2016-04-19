% resolve indentifiers namespace

validateProgram(proc(_, Addr, Args, B), proc(Addr, Args, Bv)) :-
  validateBlock(B, [], Bv).

validateBlock(b(Ds, Ins), Names, b(Dsv, Insv)) :-
  validateDeclarations(Ds, Names, NewNames, Dsv),
  validateInstructions(Ins, NewNames, Insv).

validateDeclarations(Ds, Names, NewNames, Dsv) :-
  validateDeclarations(Ds, -1, Names, NewNames, Dsv).

 % TODO check for duplicate names

% duplicateInScope(L, [H|T]) :-

validateDeclarations([], _, Names, Names, []).
validateDeclarations([H|T], Cnt, Names, Ret, Dsv) :-
  ( H = local(Id, Cnt), !,
      Cntr is Cnt - 1,
      % not(duplicateInScope(H, Names)).
      NewNames = [H| Names],
      Dsv = [local(Cnt)| Tv]
  ; H = proc(Id, Addr, Args, Block), !,
      Cntr is Cnt,
      validateFormalParameters(Args, Argsv),
      length(Args, ArgsC),
      append(Args, [par(Id, Addr, ArgsC, Args)| Names], BlockNames),
      validateBlock(Block, BlockNames, Blockv),
      NewNames = [proc(Id, Addr, ArgsC, Args)| Names],
      Dsv = [proc(Addr, Argsv, Blockv)| Tv]
  ), validateDeclarations(T, Cntr, NewNames, Ret, Tv).

 % TODO check for duplicate names

validateFormalParameters(Args, Argsv) :-
  validateFormalParameters(Args, 3, Argsv).

validateFormalParameters([], _, []).
validateFormalParameters([H| T], Cnt, [Hv|Tv]) :-
  ( H = value(_Id, Cnt), !,
      Cntr is Cnt + 1,
      Hv = value(Cnt)
  ; H = name(_Id, Cnt),
      Cntr is Cnt + 2,
      Hv = name(Cnt)
  ), validateFormalParameters(T, Cntr, Tv).

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

validateProc(procCall(Id, Addr, Params), Names, procCall(NewAddr, Paramsv)) :-
  length(Params, ParamsC),
  validateProcId(Id, Addr, FormalParams, ParamsC, Names, NewAddr), !,
  validateActualParams(Params, FormalParams, Names, Paramsv).

validateProcId(Id, Addr, FormalParams, ParamsC, [N| _], NewAddr) :-
  ( N = proc(Id, Addr, ParamsC, FormalParams), !, NewAddr = Addr
  ; N = par(Id, Addr, ParamsC, FormalParams), NewAddr = env(Addr)
  ), !.
validateProcId(Id, Addr, FormalParams, ParamsC, [N| Names], NewAddr) :-
  ( N = par(_, _, _, _), !, NewAddr = env(RetAddr)
  ; RetAddr = NewAddr
  ),
  validateProcId(Id, Addr, FormalParams, ParamsC, Names, RetAddr).

validateActualParams([], _, _, []).
validateActualParams([H| T], [P|Params], Names, [Hv| Tv]) :-
  validateExpr(H, Names, Expv),
  P =.. [Type| _],
  Hv =.. [Type, Expv],
  validateActualParams(T, Params, Names, Tv).

% validateVar(V, Names, Vv) :-
%   validateVar(V, Names, Vv).
validateVar(variable(Id, Addr), [N| _Names], V) :-
  ( N = local(Id, Addr), !, V = variable(Addr)
  ; N = name(Id, Addr), !, V = thunkCall(Addr)
  ; N = value(Id, Addr), V = variable(Addr)
  ).
validateVar(variable(Id, Addr), [N| Names], Vv) :-
  validateVar(variable(Id, Addr), Names, V), !,
  ( N = par(_, _, _, _), !,
      NewAddr = env(VAddr),
      V =.. [Key, VAddr],
      Vv =.. [Key, NewAddr]
  ; Vv = V
  ).

validate(String, Validated) :-
  parse(String, AST),
  validateProgram(AST, Validated).
