% parser

:- use_module(library(lists)).

parse(TokenList, AST) :-
  phrase(program(AST), TokenList).

program(AST) -->
  [tProgram, tIdent(Id)], block(Declarations, Instructions),
  { AST = proc(Id, [], Declarations, Instructions) }.

block(Flat, Instr) -->
  declarations(Decl), [tBegin], instructions(Instr), [tEnd],
  { flatten(Decl, Flat) }.

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
  variableIdent(variable(Id)),
  ( [tComma], !, variables(Rest),
    { Vars = [local(Id)| Rest] }
  ; [],
    { Vars = [local(Id)] }
  ).

variableIdent(V) -->
  [tIdent(Id)], { V = variable(Id) }.

procedure(P) -->
  [tProcedure, tIdent(Id), tLPar], formalParameters(Args), [tRPar],
  block(Declarations, Instructions),
  { P = proc(Id, Args, Declarations, Instructions) }.

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
  ( variableIdent(variable(Id)), !,
    { A = name(Id) }
  ; [tValue], variableIdent(variable(Id)),
    { A = value(Id) }
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
  term(First), arithExpression(First, Exp).

arithExpression(Acc, Expr) -->
  additiveOp(Op), !, term(Second),
    { Acc1 =.. [Op, Acc, Second] },
    arithExpression(Acc1, Expr).
arithExpression(Acc, Acc) -->
  [].

term(T) -->
  factor(First), term(First, T).
term(Acc, T) -->
  multiplicativeOp(Op), !, factor(Second),
    { Acc1 =.. [Op, Acc, Second] },
    term(Acc1, T).
term(Acc, Acc) -->
  [].

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
  { Call = procCall(Id, Params) }.

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
  conjunction(First), boolExpression(First, Bool).
boolExpression(Acc, Bool) -->
  [tOr], !, conjunction(Rest),
    { Acc1 = or(Acc, Rest) },
    boolExpression(Acc1, Bool).
boolExpression(Acc, Acc) -->
  [].

conjunction(Con) -->
  condition(First), conjunction(First, Con).
conjunction(Acc, Con) -->
  [tAnd], condition(Rest),
    { Acc1 = and(Acc, Rest) },
    conjunction(Acc1, Con).
conjunction(Acc, Acc) -->
  [].

condition(Con) -->
  ( [tNot], !, relationalExpression(Exp),
    { Con = not(Exp) }
  ; relationalExpression(Exp),
    { Con = Exp }
  ).

relationalExpression(Expr) -->
  ( arithExpression(Left), relationalOperator(Op), arithExpression(Right),
    { Expr =.. [Op, Left, Right] }
  ; [tLPar], !, boolExpression(Expr), [tRPar]
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
multiplicativeOp(mod) -->
  [tMod],   !.

additiveOp(add) -->
  [tPlus],  !.
additiveOp(sub) -->
  [tMinus], !.
