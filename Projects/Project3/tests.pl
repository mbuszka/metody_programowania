 % tests

testNamespace1(R) :- parseFile('test/test-1.algol16', A), validate(A, R).
testNamespace2(R) :- parseFile('test/test-2.algol16', A), validate(A, R).
testNamespace3(R) :- parseFile('test/test-3.algol16', A), validate(A, R).
testNamespace4(R) :- parseFile('test/test-4.algol16', A), validate(A, R).
testNamespace5(R) :- parseFile('test/test-5.algol16', A), validate(A, R).
testNamespace6(R) :- parseFile('test/test-6.algol16', A), validate(A, R).
testNamespace7(R) :- parseFile('test/test-7.algol16', A), validate(A, R).
testNamespace8(R) :- parseFile('test/test-8.algol16', A), validate(A, R).

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
