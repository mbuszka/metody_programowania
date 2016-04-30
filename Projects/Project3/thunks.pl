% resolveThunks works in two modes, first if given Flag as a free variable
% it will try to find which call by name parameters should be passed as a thunk,
% and which as a reference.
% In second mode, given Flag = add_references it will set all free Type variables
% to reference, works in conjunction with resolver

resolver(List, Resolved) :-
  resolveThunks(List, Flag, NewList),
  ( var(Flag), !,
    resolveThunks(NewList, add_references, Resolved)
  ; Flag = resolved,
    resolver(NewList, Resolved)
  ).

resolveThunks( [], _, []).
resolveThunks( [ H | T ], Flag, [H | New] ) :-
  H =.. [_, _, _Args, _Locals, Instr ],
  resolveThunksInstructions(Instr, Thunks, Flag),
  append(Thunks, T, All),
  resolveThunks(All, Flag, New).

resolveThunksProcCall(procCall(_Addr, Params), Thunks, Flag) :-
  resolveThunksParams(Params, Thunks, Flag).

resolveThunksParams([], [], _).
resolveThunksParams([P | Params], Thunks, Flag) :-
  ( P = value(_), !, Thunks = Thunks2
  ; P = name(E, Type),
    ( E = variable(_), !, Thunks = Thunks2
    ; E = name(_, Type), !, Thunks = Thunks2,
      ( var(Type) -> Type = Type
      ; ( ground(Flag) -> ( Flag = resolved, !
                          ; Flag = add_references )
        ; true
        )
      )
    ; E = procCall(Addr, Pars), !,  % maybe do something more efficient
      ( var(Type) ->
        Type = thunk(Addr), Flag = resolved,
        resolveThunksProcCall(procCall(Addr, Pars), Thunks1, Flag),
        append([thunk(Addr, [], [], [return(E)]) | Thunks1], Thunks2, Thunks)
      ; Type = thunk(_), Thunks = Thunks2
      )
    ; (var(Type) ->
        Type = thunk(Addr), resolveThunksExpr(E, Thunks1, Flag),
        append([thunk(Addr, [], [], [return(E)]) | Thunks1], Thunks2, Thunks),
        Flag = resolved
      ; Type = thunk(_), Thunks = Thunks2
      )
    )
  ), resolveThunksParams(Params, Thunks2, Flag).

resolveThunksInstructions([], [], _).
resolveThunksInstructions([H|T], Thunks, Flag) :-
  resolveThunksInstruction(H, ThunksH, Flag),
  append(ThunksH, ThunksT, Thunks),
  resolveThunksInstructions(T, ThunksT, Flag).

resolveThunksInstruction(I, Thunks, Flag) :-
  ( I = if(Bool, Then, Else), !,
      resolveThunksBool(Bool, ThunksB, Flag),
      resolveThunksInstructions(Then, ThunksT, Flag),
      resolveThunksInstructions(Else, ThunksE, Flag),
      append([ThunksB, ThunksT, ThunksE], Thunks)
  ; I = if(Bool, Then), !,
      resolveThunksBool(Bool, ThunksB, Flag),
      resolveThunksInstructions(Then, ThunksI, Flag),
      append(ThunksB, ThunksI, Thunks)
  ; I = while(Bool, Ins), !,
      resolveThunksBool(Bool, ThunksB, Flag),
      resolveThunksInstructions(Ins, ThunksI, Flag),
      append(ThunksB, ThunksI, Thunks)
  ; I = discardReturn(Call), !,
      resolveThunksProcCall(Call, Thunks, Flag)
  ; I = return(Exp), !,
      resolveThunksExpr(Exp, Thunks, Flag)
  ; I = ioRead(V), !,
    ( V = name(_Addr, Type), !,
      ( var(Type) -> Type = reference, Flag = resolved
      ; Type = reference )
    ; V = V
    )
  ; I = ioWrite(Exp), !,
      resolveThunksExpr(Exp, Thunks, Flag)
  ; I = assgn(V, Exp),
      resolveThunksExpr(Exp, Thunks, Flag),
      ( V = name(_Addr, Type), !,
        ( var(Type) -> Type = reference, Flag = resolved
        ; Type = reference )
      ; V = V
      )
  ).

resolveThunksExpr(Expr, Thunks, Flag) :-
  ( Expr = const(_), !,
      Thunks = []
  ; Expr = variable(_Addr), !,
      Thunks = []
  ; Expr = name(_Addr, Type), !,
      ( var(Type), ground(Flag), Flag = add_references ->
        Type = reference
      ; true
      ),
      Thunks = []
  ; Expr = procCall(_Addr, _Params), !,
      resolveThunksProcCall(Expr, Thunks, Flag)
  ; Expr = neg(E), !,
      resolveThunksExpr(E, Thunks, Flag)
  ; Expr =.. [_, Left, Right], !,
      resolveThunksExpr(Left, ThunksL, Flag),
      resolveThunksExpr(Right, ThunksR, Flag),
      append(ThunksL, ThunksR, Thunks)
  ).

resolveThunksBool(Bool, Thunks, Flag) :-
  ( Bool = not(B), !,
      resolveThunksBool(B, Thunks, Flag)
  ; Bool = and(Left, Right), !,
      resolveThunksBool(Left, ThunksL, Flag),
      resolveThunksBool(Right, ThunksR, Flag),
      append(ThunksL, ThunksR, Thunks)
  ; Bool = or(Left, Right), !,
      resolveThunksBool(Left, ThunksL, Flag),
      resolveThunksBool(Right, ThunksR, Flag),
      append(ThunksL, ThunksR, Thunks)
  ; Bool =.. [_Op, Left, Right], !,
      resolveThunksExpr(Left, ThunksL, Flag),
      resolveThunksExpr(Right, ThunksR, Flag),
      append(ThunksL, ThunksR, Thunks)
  ).
