generateThunks(Procedures, All) :-
  deduceTypes(Procedures),
  phrase(generateThunks(Procedures), All).

generateThunks([]) --> [].
generateThunks([proc(Addr, Args, Locals, Instructions) | T]) -->
  { length(Args, ArgsC), length(Locals, LocalC) },
  [ proc(Addr, ArgsC, LocalC, InstructionsCleaned)],
  generateThunksInstructions(Instructions, InstructionsCleaned),
  generateThunks(T).

generateThunksProcCall(procCall(Addr, Params), procCall(env(Addr), ParamsThunk),
                       procCall(Addr, ParamsClean)) -->
  generateThunksParams(Params, ParamsThunk, ParamsClean).

generateThunksParams([], [], []) --> [].
generateThunksParams([H | T], [HT | TT], [HC | TC]) -->
  ( { H = value(Exp) }, !,
    generateThunksExpr(Exp, ExpThunk, ExpClean),
    { HT = value(ExpThunk),
      HC = value(ExpClean) }
  ; { H = name(V, reference) }, !,
    generateThunksExpr(V, VThunk, VClean),
    { HT = reference(VThunk),
      HC = reference(VClean) }
  ; { H = name(Exp, thunk) }, !,
    generateThunksExpr(Exp, ExpThunk, _ExpClean),
    [ proc(Addr, 0, 0, [ return(ExpThunk) ]) ],
    { HT = thunk(Addr),
      HC = thunk(Addr) }
  ),
  generateThunksParams(T, TT, TC).

generateThunksInstructions([], []) --> [].
generateThunksInstructions([H | T], [HC | TC]) -->
  ( { H = if(Bool, Then, Else) }, !,
      generateThunksBool(Bool, BoolClean),
      generateThunksInstructions(Then, ThenClean),
      generateThunksInstructions(Else, ElseClean),
      { HC = if(BoolClean, ThenClean, ElseClean) }
  ; { H = if(Bool, Then) }, !,
      generateThunksBool(Bool, BoolClean),
      generateThunksInstructions(Then, ThenClean),
      { HC = if(BoolClean, ThenClean) }
  ; { H = while(Bool, Ins) }, !,
      generateThunksBool(Bool, BoolClean),
      generateThunksInstructions(Ins, InstrClean),
      { HC = while(BoolClean, InstrClean) }
  ; { H = discardReturn(Call) }, !,
      generateThunksProcCall(Call, _, CallClean),
      { HC = discardReturn(CallClean) }
  ; { H = return(Exp) }, !,
      generateThunksExpr(Exp, _, ExpClean),
      { HC = return(ExpClean) }
  ; { H = ioRead(V), !, HC = ioRead(VC),
      ( V = name(Addr, reference), !, VC = reference(Addr)
      ; V = VC ),
      HC = ioRead(VC) }
  ; { H = ioWrite(Exp) }, !,
      generateThunksExpr(Exp, _, ExpClean),
      { HC = ioWrite(ExpClean) }
  ; { H = assgn(V, Exp) },
      generateThunksExpr(Exp, _, ExpClean),
      { ( V = name(Addr, reference), !, VC = reference(Addr)
        ; V = VC ),
        HC = assgn(VC, ExpClean) }
  ),
  generateThunksInstructions(T, TC).

  generateThunksExpr(Expr, ThunkExpression, CleanedExpression) -->
    ( { Expr = const(_), !,
        ThunkExpression = Expr,
        CleanedExpression = Expr }
    ; { Expr = variable(Addr), !,
        ThunkExpression = variable(env(Addr)),
        CleanedExpression = Expr }
    ; { Expr = name(Addr, Type), !,
        ThunkExpression =.. [ Type, env(Addr) ],
        CleanedExpression =.. [ Type, Addr ] }
    ; { Expr = procCall(_Addr, _Params) }, !,
      generateThunksProcCall(Expr, ThunkExpression, CleanedExpression)
    ; { Expr = neg(E) }, !,
        generateThunksExpr(E, EThunk, EClean),
      { ThunkExpression = neg(EThunk),
        CleanedExpression = neg(EClean) }
    ; { Expr =.. [Op, Left, Right] }, !,
        generateThunksExpr(Left, LeftThunk, LeftClean),
        generateThunksExpr(Right, RightThunk, RightClean),
      { ThunkExpression =.. [ Op, LeftThunk, RightThunk ],
        CleanedExpression =.. [ Op, LeftClean, RightClean ] }
    ).

  generateThunksBool(Bool, BoolClean) -->
    ( { Bool = not(B) }, !,
        generateThunksBool(B, BClean),
        { BoolClean = not(BClean) }
    ; { Bool = and(Left, Right) }, !,
        generateThunksBool(Left, LeftClean),
        generateThunksBool(Right, RightClean),
        { BoolClean = and(LeftClean, RightClean) }
    ; { Bool = or(Left, Right) }, !,
        generateThunksBool(Left, LeftClean),
        generateThunksBool(Right, RightClean),
        { BoolClean = or(LeftClean, RightClean) }
    ; { Bool =.. [ Op, Left, Right ] }, !,
        generateThunksExpr(Left, _, LeftClean),
        generateThunksExpr(Right, _, RightClean),
        { BoolClean =.. [ Op, LeftClean, RightClean ] }
    ).
