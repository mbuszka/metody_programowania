 % generate ASM
 % because of signed arithmetic, stack addresses are treated as negative
 % FFFF is addr of stack ptr, FFFE is addr of base ptr
 % FFFD is return register,   FFFC is temporary register

generateProgram(Procedures, Assembler) :-
  phrase(generateProgram(Procedures), Assembler).

push -->                                % push acc onto the stack
  [ swapd ], load_stack_ptr,            % preserve acc, load stack_ptr into acc
  [ swapd, swapa, const(0xFFFF), add,   % add -1 to stack_ptr
    swapa, store, const(0xFFFF), swapa, % store value, store stack_ptr
    store ].

pop -->                                 % pop stack, preserves data register
  load_stack_ptr,
  [ swapd, swapa, const(1), add,   % preserve data register and
                                   %   increment stack_ptr
    swapa, swapd, const(0xFFFF),   % store stack_ptr
    swapa, store ].

top -->                       % load value from top of the stack into acc
  load_stack_ptr,
  [ swapa, load ].

update_top -->                % update top of the stack with the value in acc
  [ swapd ], load_stack_ptr,
  [ swapa, swapd, store].

generateProgram(Procedures) -->  % first procedure in list is assumed to be program
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
  [ label(Addr) ],
  ( { LocalC == 0 }, !                      % do nothing if there are no locals
  ; [ const(LocalC), swapd ], load_stack_ptr,
    [ sub ], store_stack_ptr                % or reserve space for them
  ),
  generateInstructions(Instructions, End),
  [ const(0x0000) ], store_ret_reg,         % set return value to 0 if
  [ label(End) ],                           % control reaches end of procedure
  { StackPtrOffset is ArgC + 3 },
  load_base_ptr, store_tmp_reg,             % save current base_ptr
  load_tmp_reg, offset(StackPtrOffset),
  store_stack_ptr,                          % restore stack_ptr
  load_tmp_reg, offset(0x0001),
  [ swapa, load ], store_base_ptr,          % restore base_ptr
  load_tmp_reg, offset(0x0002),
  [ swapa, load, jump ].                    % jump to return addr


generateExpression(Expr) -->
  ( { Expr = const(Value) }, !,
      [ const(Value) ], push
  ; { Expr = variable(_Addr) }, !,
      load_value(Expr), push
  ; { Expr = reference(_Addr) }, !,
      load_value(Expr), push
  ; { Expr = thunk(Addr) }, !,
      [ const(Return) ], push,               % push return address
      load_base_ptr, push,                   % push base_ptr
      jump_links(Addr, Offset),              % jump static links
      store_tmp_reg,
      offset(1), [ swapa, load ], push,      % push environment for the thunk
      load_stack_ptr, store_base_ptr,        % change base_ptr
      load_tmp_reg, offset(Offset),
      [ swapa, load, jump ],                 % jump to thunk
      [ label(Return) ],
      load_ret_reg, push                     % push return value
  ; { Expr = procCall(Label, Params) }, !,
      pushParams(Params),
      [ const(Return) ], push,               % push return address
      load_base_ptr, push,                   % push base_ptr
      jump_links(Label, Addr), push,         % push static environment
      load_stack_ptr, store_base_ptr,        % change base_ptr
      [ const(Addr), jump ],                 % jump to procedure
      [ label(Return) ],
      load_ret_reg, push                     % push return value
  ; { Expr = neg(E) }, !,
      generateExpression(E),
      top, [ swapd, const(0x0000), sub ],    % subtract value from 0
      update_top
  ; { Expr = mod(Left, Right) }, !,
      generateExpression(Left),
      generateExpression(Right),
      top, [ swapd ], pop, top,
      [ div, const(0xFFF0), swapd, shift ],  % get remainder from higher 16bits
      update_top                             % of accumulator
  ; { Expr =.. [Op, Left, Right] }, !,
      generateExpression(Left),
      generateExpression(Right),
      top, [ swapd ], pop, top,              % do arithmetic
      [ Op ], update_top
  ).

pushParams([]) --> [].
pushParams([H | T]) -->
  pushParams(T),
  ( { H = value(Expr) }, !,                 % calculate value
    generateExpression(Expr)
  ; { H = reference(V) }, !,                % push reference of a variable
    load_reference(V), push
  ; { H = thunk(Addr), !, unpackAddr(Addr, Unpacked) },
    [ const(Unpacked) ], push               % push thunk address
  ).

generateBool(Bool, True, False) -->         % True, False are addresses to which
  ( { Bool = not(B) }, !,                   % code should jump depending on
    generateBool(B, False, True)            % its evaluation.
  ; { Bool = and(Left, Right) }, !,         % due to impleme ntation, boolean
    generateBool(Left, Mid, False),         % expressions will short-circuit
    [ label(Mid) ],
    generateBool(Right, True, False)
  ; { Bool = or(Left, Right) }, !,
    generateBool(Left, True, Mid),
    [ label(Mid) ],
    generateBool(Right, True, False)
  ; { Bool = eq(Left, Right) }, !,
    generateExpression(sub(Left, Right)),   % subtract operands
    top, [ swapd ], pop, [ swapd ],
    [ swapa, const(True), swapa ],          % if they were the same, jump to True
    [ branchz, const(False), jump ]         % otherwise jump to False
  ; { Bool = nq(Left, Right) }, !,
    generateBool(eq(Left, Right), False, True)  % not-equal is not equal
  ; { Bool = gt(Left, Right) }, !,
    generateExpression(Left),               % evaluate operands
    generateExpression(Right),
    top, store_tmp_reg, pop,                % save Right in temporary register
    [ const(1), swapd ],
    load_tmp_reg, [ shift ],                % shift most significant (sign) bit
    top, [ swapd ],                         % to the left
    load_tmp_reg, [ sub, swapd ],           % subtract Left from Right
    [ const(0xFFFF), swapd, shift, swapa,   % shift one bit to right and get
      const(True), swapa, branchn,          % the sign of subtraction
      const(False), jump ]
  ; { Bool = gq(Left, Right) }, !,
    generateBool(not(lt(Left, Right)), True, False)
  ; { Bool = lt(Left, Right) }, !,
    generateExpression(Left),               % evaluate operands
    generateExpression(Right),
    top, store_tmp_reg, pop,                % save Right in temporary register
    [ const(1), swapd ],
    top, [ shift ],                         % shift most significant (sign) bit
    load_tmp_reg, [ swapd ],                % to the left
    top, [ sub, swapd ],                    % subtract Right from Left
    [ const(0xFFFF), swapd, shift, swapa,   % shift one bit to right and get
      const(True), swapa, branchn,          % the sign of subtraction
      const(False), jump ]
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
    load_reference(Var), store_tmp_reg,      % load variable reference and store
    top, [ swapd ], load_tmp_reg,            % in temporary register, then load
    [ swapa, swapd, store ], pop             % new value and store in the ref
  ).

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
