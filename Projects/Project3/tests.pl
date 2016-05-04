 % tests
testParser1(Result) :-
  parse('
    program Suma
      local x, s
    begin
      s := 0;
      read x;
      while x <> 0 do
        s := s + x;
        read x
      done;
      write s
    end',
    Result).

testParser2(Result) :-
  parse('
  program gcd
    local x, y, t
  begin
    read x;
    read y;
    while y <> 0 do
      t := y;
      y := x mod y;
      x := t
    done;
    write y
  end',
  Result).

testParser3(Result) :-
  parse('
  program Piec_Dziesiec_Pietnascie
    procedure f (x)
      procedure one ()
      begin
        write 5;
        return 1
      end
    begin
      if x > 0
      then
        call f (x - one())
      fi
    end
  begin
    call f (5)
  end',
  Result).

testParser4(Result) :-
  parse('
  program Jensen
    procedure sum (expr, index)
      local result
    begin
      result := 0;
      while index > 0 do
        result := result + expr;
        index := index - 1
      done;
      return result
    end
    local n
  begin
    n := 5;
    write sum (n * n + 1, n)
  end',
  Result).

testParser5(Result) :-
  parse('
  program Test5
    local n, m
    procedure f (x, value y)
    begin
      n := n + x;
      m := x + y
    end
  begin
    n := f(4,4)
  end',
  Result).

testParser6(Result) :-
  parse('
  program Test6
    local n, m
    procedure f (x)
      procedure g (y)
      begin
        return y + 1
      end
    begin
      return g(x) + 1
    end
  begin
    write f (n)
  end',
  Result).

testParser7(Result) :-
  parse('
  program test7
    local a
    procedure f (x1, x2, x3, x4, x5)
      local y
    begin
      (*x1 := 4;*)
      call f (y, x1, x2, x3, x4)
    end
  begin
    call f (a*a, a, a, a, a)
  end',
  Result).

testParser8(Result) :-
  parse('
  program test7
    local a
    procedure f (x1, x2, x3, x4, x5)
      local y
    begin
      x1 := 4;
      call f (x2, x3, x4, x5, y)
    end
  begin
    call f (a, a, a, a, a)
  end',
  Result).

testNamespace1(R) :- testParser1(A), phrase(validateProgram(A), R).
testNamespace2(R) :- testParser2(A), phrase(validateProgram(A), R).
testNamespace3(R) :- testParser3(A), phrase(validateProgram(A), R).
testNamespace4(R) :- testParser4(A), phrase(validateProgram(A), R).
testNamespace5(R) :- testParser5(A), phrase(validateProgram(A), R).
testNamespace6(R) :- testParser6(A), phrase(validateProgram(A), R).
testNamespace7(R) :- testParser7(A), phrase(validateProgram(A), R).
testNamespace8(R) :- testParser8(A), phrase(validateProgram(A), R).

testAsm1(Asm) :- phrase(testAsm1_, Asm), !, assemble(Asm, Words), !, saveHex(Words, 'test/test.hex').
testAsm2(Asm) :- phrase(testAsm2_, Asm), !, assemble(Asm, Words), !, saveHex(Words, 'test/test.hex').
testAsm3(Asm) :- phrase(testAsm3_, Asm), !, assemble(Asm, Words), !, saveHex(Words, 'test/test.hex').
testAsm4(Asm) :- phrase(testAsm4_, Asm), !, assemble(Asm, Words), !, saveHex(Words, 'test/test.hex').
testAsm5(Asm) :- phrase(testAsm5_, Asm), !, assemble(Asm, Words), !, saveHex(Words, 'test/test.hex').

testAsm1_ -->
  generateHeader, !,
  load_stack_ptr, [ swapd, const(2), syscall ],
  load_base_ptr, [ swapd, const(2), syscall ],
  push, load_stack_ptr, [ swapd, const(2), syscall ],
  push, load_stack_ptr, [ swapd, const(2), syscall ],
  push, load_stack_ptr, [ swapd, const(2), syscall ],
  pop, load_stack_ptr, [ swapd, const(2), syscall ],
  pop, load_stack_ptr, [ swapd, const(2), syscall ],
  pop, load_stack_ptr, [ swapd, const(2), syscall ],
  [ const(0), syscall ].

testAsm2_ -->
  generateHeader, !,
  generateInstruction(ioWrite(const(10)), 15),
  load_stack_ptr, [ swapd, const(2), syscall ],
  [ const(0), syscall ].

testAsm3_ -->
  generateHeader, !,
  [ const(3) ], push,
  [ const(4) ], push,
  [ const(5) ], push,
  top, [ swapd, const(2), syscall ],
  pop, [ const(2), syscall ],
  top, [ swapd, const(2), syscall ],
  pop, [ const(2), syscall ],
  top, [ swapd, const(2), syscall ],
  pop, [ const(2), syscall ],
  [ const(6) ], push,
  top, [ swapd, const(2), syscall ],
  load_stack_ptr, [ swapd, const(2), syscall ],
  [ const(7) ], update_top,
  load_stack_ptr, [ swapd, const(2), syscall ],
  top, [ swapd, const(2), syscall ],
  [ const(0), syscall ].

testAsm4_ -->
  generateHeader, !,
  % load_stack_ptr, [ swapd, const(2), syscall ],
  generateInstruction(ioWrite(add(const(5), const(17))), tmp),
  % load_stack_ptr, [ swapd, const(2), syscall ],
  [ const(3) ], push,
  [ const(4) ], push,
  jump_static_links(-1, Offset),
  offset(Offset),
  [ swapd, const(2), syscall ],
  [ load ], push, top, [ swapd, const(2), syscall ],
  generateExpression(variable(-1)),
  top, [ swapd, const(2), syscall ],
  generateInstruction(ioWrite(add(variable(-1), variable(-2))), tmp),
  [ const(0), syscall ].


testAsm5_ -->
  generateHeader, !,
  [ label(_), label(_), label(_), label(_) ],
  generateInstruction(ioWrite(add(const(1340), const(2))), tmp),
  [ const(Label), jump],
  % generateInstruction(ioWrite(const(12)), tmp),
  generateExpression(add(const(20), const(102))), top,
  [ swapd, const(2), syscall, nop, nop, nop ],
  [ label(Label), const(15) ],
  generateInstruction(ioWrite(add(const(30), const(12))), tmp),
  [ const(0), syscall ].

testAsm6(Procs, Asm) :-
  validate('program Test local x begin x := 5; write x + 10; write 4 mod 3 end', Procs),
  deduceTypes(Procs),
  generateThunks(Procs, Resolved),
  phrase(generateProgram(Resolved), Asm),
  assemble(Asm, Words),
  saveHex(Words, 'test/test.hex').

testAsm7(Procs, Asm) :-
  validate('program Test local a,b begin  a := 5; b := 10; if a > b then write a else write b mod a fi end', Procs),
  deduceTypes(Procs),
  generateThunks(Procs, Resolved),
  phrase(generateProgram(Resolved), Asm),
  assemble(Asm, Words),
  saveHex(Words, 'test/test.hex').
