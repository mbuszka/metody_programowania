% parser

program(AST) -->
  [tProgram, tIdent(_)], block(AST).

block(Block) -->
  declarations(Decl), [tBegin], instructions(Instr), [tEnd],
  { Block = b(Decl, Instr) }.

declarations(Declarations) -->
  declaration(First),
  ( declarations(Rest), !,
    { Declarations = [First| Rest] }
  ; [],
    { Declarations = [First] }
  ).

declaration(Decl) -->
  ( declarator(D), !, { Decl = D }
  ; procedure(P),  !, { Decl = P }
  ).

declarator(D) -->
  [tLocal], variables(V), { D = V }.

variables(Vars) -->
  variable(First),
  ( [tComma], !, variables(Rest),
    { Vars = [First| Rest] }
  ; [],
    { Vars = [First]}
  ).

variable(V) -->
  [tIdent(Id)], { V = var(Id) }.

procedure(P) -->
  [tProcedure, tIdent(Id), tLPar], formalParameters(Args), [tRPar], block(B),
  { P = proc(Id, Args, B) }.

formalParameters(Args) -->
  ( formalParametersSeq(Args), !
  ; [],
    { Args = [] }
  ).

formalParametersSeq(Args) -->
  formalParameter(First),
  ( [tComma], !, formalParametersSeq(Rest),
    { Args = [First| Rest] }
  ; [],
    { Args = [First] }
  ).

formalParameter(A) -->
  ( variable(V), !,
    { A = byName(V) }
  ; [tValue], variable(V),
    { A = byValue(V) }
  ).

instructions(IS) -->
  instruction(First),
  ( [tSCol], !, instructions(Rest),
    { IS = [First| Rest] }
  ; [],
    { IS = [First] }
  ).

instruction(I) -->
  ( [tIf], !, boolExpression(Bool), [tThen], instructions(Then),
    ( [tElse], !, instructions(Else), [tFi],
      { I = if(Bool, Then, Else) }
    ; [tFi],
      { I = if(Bool, Then) }
    )
  ; [tWhile], !, boolExpression(Bool), [tDo], instructions(Ins), [tDone],
    { I = while(Bool, Ins) }
  ; [tCall], !, procedureCall(Call),
    { I = discardReturn(Call) }
  ; [tReturn], !, arithExpression(Exp),
    { I = return(Exp) }
  ; [tRead], !, variable(V),
    { I = ioRead(V) }
  ; [tWrite], !, arithExpression(Exp),
    { I = ioWrite(Exp) }
  ; variable(V), [tAssgn], arithExpression(Exp),
    { I = assgn(V, Exp) }
  ).

arithExpression(Exp) -->
  term(First),
  ( additiveOp(Op), !, arithExpression(Second),
    { Exp =.. [Op, First, Second] }
  ; [],
    { Exp = First }
  ).

term(T) -->
  factor(First),
  ( multiplicativeOp(Op), !, term(Second),
    { T =.. [Op, First, Second] }
  ; [],
    { T = First }
  ).

factor(F) -->
  ( [tMinus], !, primary(P),
    { F = neg(P) }
  ; primary(P),
    { F = P }
  ).

primary(P) -->
  ( [tLPar], !, arithExpression(Exp), [tRPar],
    { P = Exp }
  ; variable(V), !,
    { P = V }
  ; procedureCall(Call), !,
    { P = Call}
  ; [tNum(N)],
    { P = const(N) }
  ).

procedureCall(Call) -->
  [tIdent(Id), tLPar], actualParameters(Params), [tRPar],
  { Call = call(Id, Params) }.

actualParameters(Params) -->
  ( actualParametersSeq(Params), !
  ; [],
    { Params = [] }
  ).

actualParametersSeq(Params) -->
  arithExpression(First),
  ( actualParametersSeq(Rest), !,
    { Params = [First| Rest] }
  ; [],
    { Params = [First] }
  ).

multiplicativeOp(mul) -->
  [tTimes], !.
multiplicativeOp(div) -->
  [tDiv],   !.
additiveOp(add) -->
  [tPlus],  !.
additiveOp(sub) -->
  [tMinus], !.
