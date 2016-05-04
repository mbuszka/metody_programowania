 % generate ASM
 % because of signed arithmetic, stack addresses are treated as negative
 % numbers !!!

generate(Procedures, Assembler) :-
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

generateProgram([proc(Addr, Params, Locals, Instructions)| Procedures]) -->
  generateHeader,
  generateExpression(procCall(Addr, [])),
  [ const(0), syscall ],
  generateProcedure(proc(Addr, Params, Locals, Instructions)).

generateHeader -->
  [ const(0xFFFB) ], store_stack_ptr,
  [ const(0xFFFB) ], store_base_ptr.

generateProcedure(proc(Addr, Args, Locals, Instructions)) -->
  [ label(Addr) ],          % set label
  %  [ const(42), swapd, const(2), syscall ],
  { length(Args, ArgC), StackPtrOffset is ArgC + 3, length(Locals, LocalC) },
  [ const(LocalC), swapd ],
  load_stack_ptr, [ add ], store_stack_ptr,         % set stack_ptr on top of the locals
  generateInstructions(Instructions, End),
  [ const(0x0000) ], store_ret_reg,                 % set return value to 0 if control reaches end of procedure
  [ label(End) ],
  load_base_ptr, offset(StackPtrOffset), store_stack_ptr, % restore stack_ptr
  load_base_ptr, offset(0x0002), [ swapa, load ],       % store return addr
  [ swapd, const(2), syscall, swapd ],
  store_tmp_reg,
  load_base_ptr, offset(0x0001), [ swapa, load ],   % restore base_ptr
  store_base_ptr,
  load_ret_reg, push,                               % push return value
  load_tmp_reg,
  % load_base_ptr, offset(0x0002), [ swapa, load, swapd, const(2), syscall, swapd ],
  [ jump ].                                         % jump to return addr

generateExpression(Expr) -->
  ( { Expr = const(Value) }, !,
      [ const(Value) ], push
  ; { Expr = variable(Addr) }, !,
      jump_static_links(Addr, Offset),
      offset(Offset),
      [ swapa, load ], push
  ; { Expr = thunkCall(Addr) }, !
  ; { Expr = procCall(Label, Params) }, !,
      pushParams(Params),
      [ const(Return) ], [ swapd, const(2), syscall, swapd ], push,
      load_base_ptr, push,
      load_base_ptr, jump_static_links(Label, Addr), push,
      load_stack_ptr, store_base_ptr,
      % [ const(41), swapd, const(2), syscall ],
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

generateBool(Bool, True, False) -->
  ( { Bool = not(B) }, !
  ; { Bool = and(Left, Right) }, !
  ; { Bool = or(Left, Right) }, !
  ; { Bool = eq(Left, Right) }, !
  ; { Bool = nq(Left, Right) }, !
  ; { Bool = gt(Left, Right) }, !
  ; { Bool = gq(Left, Right) }, !
  ; { Bool = lt(Left, Right) }, !
  ; { Bool = lq(Left, Right) }, !
  ).

generateInstructions([], _End) -->
  [].
generateInstructions([H|T], End) -->
  generateInstruction(H, End),
  generateInstructions(T, End).

generateInstruction(Instr, End) -->
  ( { Instr = if(Bool, Then, Else) }, !
  ; { Instr = if(Bool, Then) }, !
  ; { Instr = while(Bool, Instructions) }, !
  ; { Instr = discardReturn(Call) }, !,
    generateExpression(Call),
    top,
    [ swapd, const(2), syscall ], % TODO delete after testing
    pop
  ; { Instr = return(Expr) }, !
  ; { Instr = ioRead(Var) }, !
  ; { Instr = ioWrite(Expr) }, !,
      generateExpression(Expr),
      top, [ swapd, const(2), syscall ],
      pop
  ; { Instr = assgn(Var, Expr) }, !
  ).

 % loads the static environment base_ptr into the accumulator and returns the
 % inner value in Unpacked
jump_static_links(Addr, Unpacked) -->
  load_base_ptr, jump_static_links_(Addr, Unpacked).

jump_static_links_(Addr, Unpacked) -->
  ( { var(Addr), Unpacked = Addr }, []
  ; { Addr = env(A) }, !,
      [ swapa, load ],
      jump_static_links_(A, Unpacked)
  ; { Unpacked = Addr }, []
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
