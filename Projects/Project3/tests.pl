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
      t := x mod y;
      y := x;
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
