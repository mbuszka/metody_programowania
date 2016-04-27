 % tests
test1 -->
  generateHeader,
  load_stack_ptr, [ swapd, const(2), syscall ],
  load_base_ptr, [ swapd, const(2), syscall ],
  [ const(0), syscall ].

test2 -->
  generateHeader,
  generateInstruction(ioWrite(const(10)), 15),
  [ const(0), syscall ].

test3 -->
  generateHeader,
  generateInstruction(ioWrite(add(const(5), const(17))), tmp),
  load_stack_ptr, offset(-10), [ swapd, const(2), syscall ],
  [ const(0), syscall ].

test5 -->
  generateHeader,
  [ const(Label), jump],
  [ const(10), swapd, const(2), syscall ], % should not execute
  [ label(Label), const(15), swapd, const(2), syscall ],
  [ const(0), syscall ].

test4(Asm) :-
  resolve('program Test begin write 17 end', Procs),
  phrase(generateProgram(Procs), Asm),
  assemble(Asm, Words),
  printHex(Words).
