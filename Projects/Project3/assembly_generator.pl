 % generate ASM
 % because of signed arithmetic, stack addresses are treated as negative
 % numbers !!!

generateProgram(Procedures, Assembler) :-
  phrase(generateProgram(Procedures), Assembler).

push -->                                % push acc onto the stack
  [ swapd ], load_stack_ptr,            % preserve acc, load stack_ptr into acc
  [ swapd, swapa, const(0xFFFF), add,        % add -1 to stack_ptr
    swapa, store, const(0xFFFF), swapa, % store value, store stack_ptr
    store ].

pop -->                                 % pop stack, preserves data register
  load_stack_ptr,
  [ swapd, swapa, const(1), add,   % preserve data register, increment stack_ptr
    swapa, swapd, const(0xFFFF), swapa, % store stack_ptr
    store ].

top -->             % load value on top of the stack into acc
  load_stack_ptr,
  [ swapa, load ].

update_top -->                % update top of the stack to the value in acc
  [ swapd ], load_stack_ptr,
  [ swapa, swapd, store].

generateProgram(Procedures) -->
  { [proc(Addr, _ParamsC, _LocalsC, _Instructions)| _ ] = Procedures },
  generateHeader,
  generateExpression(procCall(Addr, [])),
  [ const(0), syscall ],
  generateProcedures(Procedures).

generateHeader -->
  [ const(0xFFFB) ], store_stack_ptr,
  [ const(0xFFFB) ], store_base_ptr.

generateProcedures([]) --> [].
generateProcedures([H | T]) -->
  generateProcedure(H),
  generateProcedures(T).

generateProcedure(proc(Addr, ArgC, LocalC, Instructions)) -->
  [ label(Addr) ],          % set label
  [ const(LocalC), swapd ],
  load_stack_ptr, [ sub ], store_stack_ptr,         % set stack_ptr on top of the locals
  generateInstructions(Instructions, End),
  [ const(0x0000) ], store_ret_reg,                 % set return value to 0 if control reaches end of procedure
  [ label(End) ],
  { StackPtrOffset is ArgC + 3 },
  load_base_ptr, offset(StackPtrOffset), store_stack_ptr, % restore stack_ptr
  load_base_ptr, offset(0x0002), [ swapa, load ],       % store return addr
  store_tmp_reg,
  load_base_ptr, offset(0x0001), [ swapa, load ],   % restore base_ptr
  store_base_ptr,
  load_ret_reg, push,                               % push return value
  load_tmp_reg,
  [ jump ].                                         % jump to return addr

generateExpression(Expr) -->
  ( { Expr = const(Value) }, !,
      [ const(Value) ], push
  ; { Expr = variable(_Addr) }, !,
      load_value(Expr), push
  ; { Expr = reference(_Addr) }, !,
      load_value(Expr), push
  ; { Expr = thunk(Addr) }, !,
      [ const(Return) ], push,
      load_base_ptr, push,
      jump_links(Addr, Offset), store_tmp_reg,
      offset(1), [ swapa, load ], push,      % load callers env
      load_stack_ptr, store_base_ptr,
      load_tmp_reg, offset(Offset), [ swapa, load, jump ],
      [ label(Return) ]
  ; { Expr = procCall(Label, Params) }, !,
      pushParams(Params),
      [ const(Return) ],
      push,
      load_base_ptr, push,
      jump_links(Label, Addr), push,
      load_stack_ptr, store_base_ptr,
      [ const(Addr), jump ],
      [ label(Return) ]
  ; { Expr = neg(E) }, !,
      generateExpression(E),
      top, [ swapd, const(0x0000), sub ],
      update_top
  ; { Expr = mod(Left, Right) }, !,
      generateExpression(Left),
      generateExpression(Right),
      top, [ swapd ], pop, top,
      [ div, const(0xFFF0), swapd, shift ],
      update_top
  ; { Expr =.. [Op, Left, Right] }, !,
      generateExpression(Left),
      generateExpression(Right),
      top, [ swapd ], pop, top,
      [ Op ], update_top
  ).

pushParams([]) --> [].
pushParams([H | T]) -->
  pushParams(T),
  ( { H = value(Expr) }, !,
    generateExpression(Expr)
  ; { H = reference(V) }, !,
    load_reference(V), push
  ; { H = thunk(Addr), !, unpackAddr(Addr, Unpacked) },
    [ const(Unpacked) ], push
  ).

generateBool(Bool, True, False) -->
  ( { Bool = not(B) }, !,
    generateBool(B, False, True)
  ; { Bool = and(Left, Right) }, !,
    generateBool(Left, Mid, False),
    [ label(Mid) ],
    generateBool(Right, True, False)
  ; { Bool = or(Left, Right) }, !,
    generateBool(Left, True, Mid),
    [ label(Mid) ],
    generateBool(Right, True, False)
  ; { Bool = eq(Left, Right) }, !,
    generateExpression(sub(Left, Right)),
    top, [ swapd ], pop, [ swapd ],
    [ swapa, const(True), swapa ],
    [ branchz, const(False), jump ]
  ; { Bool = nq(Left, Right) }, !,
    generateBool(eq(Left, Right), False, True)
  ; { Bool = gt(Left, Right) }, !,
    generateExpression(Left),
    generateExpression(Right),
    top, store_tmp_reg, pop,
    load_tmp_reg, [ swapa, const(1), swapd, swapa, shift ],
    top, [ swapd ], load_tmp_reg, [ sub, swapd ],
    [ const(0xFFFF), swapd, shift ],
    [ swapa, const(True), swapa ],
    [ branchn, const(False), jump ]
  ; { Bool = gq(Left, Right) }, !,
    generateBool(not(lt(Left, Right)), True, False)
  ; { Bool = lt(Left, Right) }, !,
    generateExpression(Left),
    generateExpression(Right),
    top, store_tmp_reg, pop,
    top, [ swapa, const(1), swapd, swapa, shift ],
    load_tmp_reg, [ swapd ], top, [ sub, swapd ],
    [ const(0xFFFF), swapd, shift ],
    [ swapa, const(True), swapa ],
    [ branchn, const(False), jump ]
  ; { Bool = lq(Left, Right) }, !,
    generateBool(not(gt(Left, Right)), True, False)
  ).

generateInstructions([], _Return) -->
  [].
generateInstructions([H|T], Return) -->
  generateInstruction(H, Return),
  generateInstructions(T, Return).

generateInstruction(Instr, Return) -->
  ( { Instr = if(Bool, Then, Else) }, !,
    generateBool(Bool, True, False),
    [ label(True) ], generateInstructions(Then, Return), [ const(End), jump ],
    [ label(False) ], generateInstructions(Else, Return),
    [ label(End) ]
  ; { Instr = if(Bool, Then) }, !,
    generateBool(Bool, True, False),
    [ label(True) ], generateInstructions(Then, Return),
    [ label(False) ]
  ; { Instr = while(Bool, Instructions) }, !,
    [ label(Begin) ],
    generateBool(Bool, True, False),
    [ label(True) ],
    generateInstructions(Instructions, Return),
    [ const(Begin), jump ],
    [ label(False) ]
  ; { Instr = discardReturn(Call) }, !,
    generateExpression(Call),
    pop
  ; { Instr = return(Expr) }, !,
    generateExpression(Expr),
    top, store_ret_reg, pop,
    [ const(Return), jump ]
  ; { Instr = ioRead(Var) }, !,
    load_reference(Var),
    [ swapa, const(1), syscall, store ]
  ; { Instr = ioWrite(Expr) }, !,
      generateExpression(Expr),
      top, [ swapd, const(2), syscall ],
      pop
  ; { Instr = assgn(Var, Expr) }, !,
    generateExpression(Expr),
    load_reference(Var), store_tmp_reg,
    top, [ swapd ], load_tmp_reg,
    [ swapa, swapd, store ], pop
  ).

print_acc --> [ swapd, const(2), syscall, swapd ].

load_reference(V) -->
  ( { V = reference(Addr) }, !,
    jump_links(Addr, Offset),
    offset(Offset), [ swapa, load ]
  ; { V = variable(Addr) }, !,
    jump_links(Addr, Offset),
    offset(Offset)
  ).

load_value(V) -->
  load_reference(V), [ swapa, load ].

 % loads the environment base_ptr into the accumulator and returns the
 % inner value in Unpacked
jump_links(Addr, Unpacked) -->
  load_base_ptr, jump_links_(Addr, Unpacked).

jump_links_(Addr, Unpacked) -->
  ( { var(Addr), !, Unpacked = Addr }, []
  ; { Addr = env(A) }, !,
      [ swapa, load ],
      jump_links_(A, Unpacked)
  ; { Unpacked = Addr }, []
  ).

unpackAddr(Addr, Unpacked) :-
  ( var(Addr), !, Unpacked = Addr
  ; Addr = env(A), !, unpackAddr(A, Unpacked)
  ; Addr = dyn(A), !, unpackAddr(A, Unpacked)
  ; Addr = Unpacked
  ).

% offset the accumulator by given value
offset(Offset) -->
  ( { Offset < 0, Of is 0x10000 + Offset } % one's complement
  ; { Of is Offset }
  ),
  [ swapd, const(Of), add ].

load_ret_reg  --> load_reg(0xFFFC).
store_ret_reg --> store_reg(0xFFFC).

load_tmp_reg  --> load_reg(0xFFFD).
store_tmp_reg --> store_reg(0xFFFD).

load_base_ptr  --> load_reg(0xFFFE).
store_base_ptr --> store_reg(0xFFFE).

load_stack_ptr  --> load_reg(0xFFFF).
store_stack_ptr --> store_reg(0xFFFF).

load_reg(Addr) -->
  [ const(Addr), swapa, load ].

store_reg(Addr) -->
  [ swapa, const(Addr), swapa, store ].
