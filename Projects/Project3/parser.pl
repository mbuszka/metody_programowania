% parser

:- use_module(library(lists)).

parse(Text, AST) :-
  atom_codes(Text, Codes),
  phrase(lexer(Tokens), Codes),
  phrase(program(AST), Tokens).

program(AST) -->
  [tProgram, tIdent(Id)], block(B),
  { AST = proc(Id, _Addr, [], B) }.

block(Block) -->
  declarations(Decl), [tBegin], instructions(Instr), [tEnd],
  { flatten(Decl, Flat),
    Block = b(Flat, Instr) }.

declarations(Declarations) -->
  ( declaration(First), !, declarations(Rest),
    { Declarations = [First| Rest] }
  ; [],
    { Declarations = [] }
  ).

declaration(Decl) -->
  ( declarator(D), !, { Decl = D }
  ; procedure(P),  !, { Decl = [P] }
  ).

declarator(D) -->
  [tLocal], variables(V), { D = V }.

variables(Vars) -->
  variableIdent(variable(Id, Addr)),
  ( [tComma], !, variables(Rest),
    { Vars = [local(Id, Addr)| Rest] }
  ; [],
    { Vars = [local(Id, Addr)] }
  ).

variableIdent(V) -->
  [tIdent(Id)], { V = variable(Id, _Addr) }.

procedure(P) -->
  [tProcedure, tIdent(Id), tLPar], formalParameters(Args), [tRPar], block(B),
  { P = proc(Id, _Addr, Args, B) }.

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
  ( variableIdent(variable(Id, Addr)), !,
    { A = name(Id, Addr) }
  ; [tValue], variableIdent(variable(Id, Addr)),
    { A = value(Id, Addr) }
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
  ; [tRead], !, variableIdent(V),
    { I = ioRead(V) }
  ; [tWrite], !, arithExpression(Exp),
    { I = ioWrite(Exp) }
  ; variableIdent(V), [tAssgn], arithExpression(Exp),
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
  ; procedureCall(Call), !,
    { P = Call }
  ; variableIdent(V), !,
    { P = V }
  ; [tNum(N)],
    { P = const(N) }
  ).

procedureCall(Call) -->
  [tIdent(Id), tLPar], actualParameters(Params), [tRPar],
  { Call = procCall(Id, _Addr, Params) }.

actualParameters(Params) -->
  ( actualParametersSeq(Params), !
  ; [],
    { Params = [] }
  ).

actualParametersSeq(Params) -->
  arithExpression(First),
  ( [tComma], !, actualParametersSeq(Rest),
    { Params = [First| Rest] }
  ; [],
    { Params = [First] }
  ).

boolExpression(Bool) -->
  conjunction(First),
  ( [tOr], !, boolExpression(Rest),
    { Bool = or(First, Rest) }
  ; [],
    { Bool = First }
  ).

conjunction(Con) -->
  condition(First),
  ( [tAnd], conjunction(Rest),
    { Con = and(First, Rest) }
  ; [],
    { Con = First }
  ).

condition(Con) -->
  ( [tNot], !, relationalExpression(Exp),
    { Con = not(Exp) }
  ; relationalExpression(Exp),
    { Con = Exp }
  ).

relationalExpression(Expr) -->
  ( [tLPar], !, boolExpression(Expr), [tRPar]
  ; arithExpression(Left), relationalOperator(Op), arithExpression(Right),
    { Expr =.. [Op, Left, Right] }
  ).

relationalOperator(lt) -->
  [tLt],  !.
relationalOperator(lq) -->
  [tLeq], !.
relationalOperator(gt) -->
  [tGt],  !.
relationalOperator(gq) -->
  [tGeq], !.
relationalOperator(eq) -->
  [tEq],  !.
relationalOperator(nq) -->
  [tNeq], !.

multiplicativeOp(mul) -->
  [tTimes], !.
multiplicativeOp(div) -->
  [tDiv],   !.
additiveOp(add) -->
  [tPlus],  !.
additiveOp(sub) -->
  [tMinus], !.
