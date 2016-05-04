% deduceTypes works in two modes, first if given Flag as a free variable
% it will try to find which call by name parameters should be passed as a thunk,
% and which as a reference.
% In second mode, given Flag = add_references it will set all free Type variables
% to reference, works in conjunction with deduce

deduce(List) :-
  deduceTypes(List, Flag),
  ( var(Flag), !,
    deduceTypes(List, add_references)
  ; Flag = resolved,
    deduce(List)
  ).

deduceTypes( [], _).
deduceTypes([H | T], Flag) :-
  H =.. [_, _, _Args, _Locals, Instr ],
  deduceTypesInstructions(Instr, Flag),
  deduceTypes(T, Flag).

deduceTypesProcCall(procCall(_Addr, Params), Flag) :-
  deduceTypesParams(Params, Flag).

deduceTypesParams([], _).
deduceTypesParams([P | Params], Flag) :-
  ( P = value(_Addr), !
  ; P = name(E, Type),
    ( E = variable(_Addr), !
    ; E = name(_Addr, Type), !,
      ( var(Type) -> Type = Type
      ; ( ground(Flag) -> ( Flag = resolved, !
                          ; Flag = add_references )
        ; true
        )
      )
    ; (var(Type) ->
        Type = thunk,
        deduceTypesExpr(E, Flag),
        Flag = resolved
      ; Type = thunk
      )
    )
  ), deduceTypesParams(Params, Flag).

deduceTypesInstructions([],  _).
deduceTypesInstructions([H|T], Flag) :-
  deduceTypesInstruction(H, Flag),
  deduceTypesInstructions(T, Flag).

deduceTypesInstruction(I, Flag) :-
  ( I = if(Bool, Then, Else), !,
      deduceTypesBool(Bool, Flag),
      deduceTypesInstructions(Then, Flag),
      deduceTypesInstructions(Else, Flag)
  ; I = if(Bool, Then), !,
      deduceTypesBool(Bool, Flag),
      deduceTypesInstructions(Then, Flag)
  ; I = while(Bool, Ins), !,
      deduceTypesBool(Bool, Flag),
      deduceTypesInstructions(Ins, Flag)
  ; I = discardReturn(Call), !,
      deduceTypesProcCall(Call, Flag)
  ; I = return(Exp), !,
      deduceTypesExpr(Exp, Flag)
  ; I = ioRead(V), !,
    ( V = name(_Addr, Type), !,
      ( var(Type) -> Type = reference, Flag = resolved
      ; Type = reference )
    ; V = V
    )
  ; I = ioWrite(Exp), !,
      deduceTypesExpr(Exp, Flag)
  ; I = assgn(V, Exp),
      deduceTypesExpr(Exp, Flag),
      ( V = name(_Addr, Type), !,
        ( var(Type) -> Type = reference, Flag = resolved
        ; Type = reference )
      ; V = V
      )
  ).

deduceTypesExpr(Expr, Flag) :-
  ( Expr = const(_), !
  ; Expr = variable(_Addr), !
  ; Expr = name(_Addr, Type), !,
      ( var(Type), ground(Flag), Flag = add_references ->
        Type = reference
      ; true
      )
  ; Expr = procCall(_Addr, _Params), !,
      deduceTypesProcCall(Expr, Flag)
  ; Expr = neg(E), !,
      deduceTypesExpr(E, Flag)
  ; Expr =.. [_Op, Left, Right], !,
      deduceTypesExpr(Left, Flag),
      deduceTypesExpr(Right, Flag)
  ).

deduceTypesBool(Bool, Flag) :-
  ( Bool = not(B), !,
      deduceTypesBool(B, Flag)
  ; Bool = and(Left, Right), !,
      deduceTypesBool(Left, Flag),
      deduceTypesBool(Right, Flag)
  ; Bool = or(Left, Right), !,
      deduceTypesBool(Left, Flag),
      deduceTypesBool(Right, Flag)
  ; Bool =.. [_Op, Left, Right], !,
      deduceTypesExpr(Left, Flag),
      deduceTypesExpr(Right, Flag)
  ).
