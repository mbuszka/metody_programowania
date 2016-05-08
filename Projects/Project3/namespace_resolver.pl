% resolve indentifiers namespace

validateProgram(proc(_, [], Ds, Ins)) -->
  [ proc(_Addr, [], Locals, Insv) ],
  validateDeclarations(Ds, [], [], NewNames, 0, Locals),
  { validateInstructions(Ins, NewNames, Insv) }.

validateProc(proc(Id, Args, Ds, Ins), Names,
             [ proc(Id, Addr, Argsv) | Names ]) -->
  { validateFormalParameters(Args, Argsv, Names1),
    append(Names1, [ par(Id, Addr, Argsv) | Names ], Names2) },
  [ proc(Addr, Argsv, Locals, Insv) ],
  validateDeclarations(Ds, Names2, Names1, NewNames, 0, Locals),
  { validateInstructions(Ins, NewNames, Insv) }.

 % TODO check for duplicate names

validateDeclarations([], Names, _LocalNames, Names, _Cnt, []) -->
  [].
validateDeclarations([H|T], Names, LocalNames, RetNames, Cnt, NewLocals) -->
  ( { H = local(Id), !,
      Cntr is Cnt - 1,
      NewNames = [ local(Id, Cntr) | Names ],
      NewLocals = [ local(Cntr) | Locals ] }
  ; validateProc(H, Names, NewNames),
  { Cntr is Cnt, NewLocals = Locals, NewLocalNames = LocalNames }
  ),
  validateDeclarations(T, NewNames, NewLocalNames, RetNames, Cntr, Locals).

 % TODO check for duplicate names

validateFormalParameters(Args, Argsv, Names) :-
  validateFormalParameters(Args, Argsv, 3, Names).

validateFormalParameters([], [], _, []).
validateFormalParameters([H| T], [Hv|Tv], Cnt, NewNames) :-
  ( H = value(Id), !,
      Cntr is Cnt + 1,
      Hv = value(Cnt),
      NewNames = [ value(Id, Cnt) | Names ]
  ; H = name(Id),
      Cntr is Cnt + 1,
      Hv = name(Cnt, Type),
      NewNames = [ name(Id, Cnt, Type) | Names ]
  ), validateFormalParameters(T, Tv, Cntr, Names).

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
      validateProcCall(Call, Names, Callv),
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
  ; Expr = variable(_Id), !,
      validateVar(Expr, Names, Exprv)
  ; Expr = procCall(_Id, _Params), !,
      validateProcCall(Expr, Names, Exprv)
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

validateProcCall(procCall(Id, Params), Names, procCall(Addr, Paramsv)) :-
  length(Params, ParamsC),
  validateProcId(Id, ParamsC, Names, Args, Addr), !,
  validateActualParams(Params, Args, Names, Paramsv).

validateProcId(Id, ParamsC, [N| _], Args, NewAddr) :-
  ( N = proc(Id, NewAddr, Args), !, length(Args, ParamsC)
  ; N = par(Id, Addr, Args), length(Args, ParamsC), NewAddr = env(Addr)
  ), !.
validateProcId(Id, ParamsC, [N| Names], Args, NewAddr) :-
  ( N = par(_, _, _), !, NewAddr = env(RetAddr)
  ; NewAddr = RetAddr
  ),
  validateProcId(Id, ParamsC, Names, Args, RetAddr).

validateActualParams([], _, _, []).
validateActualParams([H| T], [A|Args], Names, [Hv| Tv]) :-
  validateExpr(H, Names, Expv),
  ( A = name(_, Type), !,
    Hv = name(Expv, Type)
  ; A = value(_),
    Hv = value(Expv)),
  validateActualParams(T, Args, Names, Tv).

% validateVar(V, Names, Vv) :-
%   validateVar(V, Names, Vv).
validateVar(variable(Id), [N| _Names], V) :-
  ( N = local(Id, Addr), !, V = variable(Addr)
  ; N = name(Id, Addr, Type), !, V = name(Addr, Type)
  ; N = value(Id, Addr), V = variable(Addr)
  ).
validateVar(variable(Id), [N| Names], Vv) :-
  validateVar(variable(Id), Names, V), !,
  ( N = par(_, _, _), !,
      NewAddr = env(Vaddr),
      ( V = name(Type, Vaddr),
        Vv = name(Type, NewAddr)
      ; V = variable(Vaddr),
        Vv = variable(NewAddr)
      )
  ; Vv = V
  ).

validate(AST, Validated) :-
  phrase(validateProgram(AST), Validated).
