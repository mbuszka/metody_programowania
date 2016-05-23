 % Maciej Buszka
 % Compiler implements full Algol16 language
 % It's source code is split into modules, which subsequently transform
 % Algol16 source code into machine words:
 %  - parser and lexer
 %  - namespace validatation
 %  - call by name type resolving and thunk generation
 %  - assembly generation
 %  - translation into machine words

algol16(Codes, MachineWords) :-
  lex(Codes, Tokens),
  parse(Tokens, AST),
  validate(AST, Procedures),
  generateThunks(Procedures, Thunks),
  generateProgram(Thunks, Assembly),
  assemble(Assembly, MachineWords).

%  lexer based on "while_parser" by TWI

lex(CodesList, TokenList) :-
  phrase(lexer(TokenList), CodesList).

lexer(Tokens) -->
  whitespace,
  ( ":=",       !, { Token = tAssgn }
  ; ";",        !, { Token = tSCol }
  ; ",",        !, { Token = tComma }
  ; "+",        !, { Token = tPlus }
  ; "-",        !, { Token = tMinus }
  ; "*",        !, { Token = tTimes }
  ; "(",        !, { Token = tLPar }
  ; ")",        !, { Token = tRPar }
  ; "=",        !, { Token = tEq }
  ; "<>",       !, { Token = tNeq }
  ; "<=",       !, { Token = tLeq }
  ; "<",        !, { Token = tLt }
  ; ">=",       !, { Token = tGeq }
  ; ">",        !, { Token = tGt }
  ; digit(D),   !,
      number(D, N),
      { Token = tNum(N) }
  ; letter(L),  !,
      identifier(L, Id),
      { isKeyword(Id, Token), !
      ; Token = tIdent(Id)
      }
  ; [_],
      { Token = tUnknown }
  ),
  !,
  whitespace,
  { Tokens = [Token| TList] },
  lexer(TList).
lexer([]) -->
  [].

commentEnd -->
  ( "*)", !
  ; [_], commentEnd
  ).

whitespace -->
  ( "(*", !, commentEnd, whitespace
  ; [Char], { code_type(Char, space) }, !, whitespace
  ; []
  ).

digit(D) -->
  [D],
  { code_type(D, digit)}.

digits([D|T]) -->
  digit(D),
  !,
  digits(T).
digits([]) -->
  [].

number(D, N) -->
  digits(DS),
  { number_chars(N, [D| DS]) }.

letter(L) -->
  [L],
  { code_type(L, alpha) }.

identStr([L| T]) -->
  [L],
  { code_type(L, alnum) ; char_code('_', L); char_code('''', L) },
  !,
  identStr(T).
identStr([]) -->
  [].

identifier(L, Id) -->
  identStr(LS),
  { atom_codes(Id, [L| LS])}.

isKeyword(Id, Token) :-
  member(Id-Token, [ and-tAnd,
                     begin-tBegin,
                     call-tCall,
                     div-tDiv,
                     do-tDo,
                     done-tDone,
                     else-tElse,
                     end-tEnd,
                     fi-tFi,
                     if-tIf,
                     local-tLocal,
                     mod-tMod,
                     not-tNot,
                     or-tOr,
                     procedure-tProcedure,
                     program-tProgram,
                     read-tRead,
                     return-tReturn,
                     then-tThen,
                     value-tValue,
                     while-tWhile,
                     write-tWrite ]).

% parser based on "while_parser" by TWI

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

% resolve indentifiers namespace
% Names is a list of known identifiers, when checking namespace,
% first matching identifier is used (respecting type - variable or procedure)

validate(AST, Validated) :-           % function used by the rest of compiler
  phrase(validateProgram(AST), Validated).

validateProgram(proc(_, [], Ds, Ins)) -->
  [ proc(_Addr, [], Locals, Insv) ],
  validateDeclarations(Ds, [], [], NewNames, 0, Locals),
  { validateInstructions(Ins, NewNames, Insv) }.

validateProc(proc(Id, Args, Ds, Ins), Names,
             [ proc(Id, Addr, Argsv) | Names ]) -->
  { validateFormalParameters(Args, Argsv, Names1),
    append(Names1, [ par(Id, Addr, Argsv) | Names ], Names2) },
  [ proc(Addr, Argsv, Locals, Insv) ],
  validateDeclarations(Ds, Names2, Names1, NewNames, 0, Locals),
  { validateInstructions(Ins, NewNames, Insv) }.

 % TODO check for duplicate names maybe

validateDeclarations([], Names, _LocalNames, Names, _Cnt, []) -->
  [].
validateDeclarations([H|T], Names, LocalNames, RetNames, Cnt, NewLocals) -->
  ( { H = local(Id), !,
      Cntr is Cnt - 1,
      NewNames = [ local(Id, Cntr) | Names ],
      NewLocals = [ local(Cntr) | Locals ] }
  ; validateProc(H, Names, NewNames),
  { Cntr is Cnt, NewLocals = Locals, NewLocalNames = LocalNames }
  ),
  validateDeclarations(T, NewNames, NewLocalNames, RetNames, Cntr, Locals).

 % TODO check for duplicate names maybe

validateFormalParameters(Args, Argsv, Names) :-
  validateFormalParameters(Args, Argsv, 3, Names).

validateFormalParameters([], [], _, []).
validateFormalParameters([H| T], [Hv|Tv], Cnt, NewNames) :-
  ( H = value(Id), !,
      Cntr is Cnt + 1,
      Hv = value(Cnt),
      NewNames = [ value(Id, Cnt) | Names ]
  ; H = name(Id),
      Cntr is Cnt + 1,
      Hv = name(Cnt, Type),
      NewNames = [ name(Id, Cnt, Type) | Names ]
  ), validateFormalParameters(T, Tv, Cntr, Names).

validateInstructions([], _, []).
validateInstructions([H|T], Names, [Hv|Tv]) :-
  validateInstruction(H, Names, Hv),
  validateInstructions(T, Names, Tv).

validateInstruction(I, Names, Iv) :-
  ( I = if(Bool, Then, Else), !,
      validateBool(Bool, Names, Boolv),
      validateInstructions(Then, Names, Thenv),
      validateInstructions(Else, Names, Elsev),
      Iv = if(Boolv, Thenv, Elsev)
  ; I = if(Bool, Then), !,
      validateBool(Bool, Names, Boolv),
      validateInstructions(Then, Names, Thenv),
      Iv = if(Boolv, Thenv)
  ; I = while(Bool, Ins), !,
      validateBool(Bool, Names, Boolv),
      validateInstructions(Ins, Names, Insv),
      Iv = while(Boolv, Insv)
  ; I = discardReturn(Call), !,
      validateProcCall(Call, Names, Callv),
      Iv = discardReturn(Callv)
  ; I = return(Exp), !,
      validateExpr(Exp, Names, Expv),
      Iv = return(Expv)
  ; I = ioRead(V), !,
      validateVar(V, Names, Vv),
      Iv = ioRead(Vv)
  ; I = ioWrite(Exp), !,
      validateExpr(Exp, Names, Expv),
      Iv = ioWrite(Expv)
  ; I = assgn(V, Exp),
      validateVar(V, Names, Vv),
      validateExpr(Exp, Names, Expv),
      Iv = assgn(Vv, Expv)
  ).

validateExpr(Expr, Names, Exprv) :-
  ( Expr = const(_), !,
      Exprv = Expr
  ; Expr = variable(_Id), !,
      validateVar(Expr, Names, Exprv)
  ; Expr = procCall(_Id, _Params), !,
      validateProcCall(Expr, Names, Exprv)
  ; Expr = neg(E), !,
      validateExpr(E, Names, Ev),
      Exprv = neg(Ev)
  ; Expr =.. [Op, Left, Right], !,
      validateExpr(Left, Names, Leftv),
      validateExpr(Right, Names, Rightv),
      Exprv =.. [Op, Leftv, Rightv]
  ).

validateBool(Bool, Names, Boolv) :-
  ( Bool = not(B), !,
      validateBool(B, Names, Bv),
      Boolv = not(Bv)
  ; Bool = and(Left, Right), !,
      validateBool(Left, Names, Leftv),
      validateBool(Right, Names, Rightv),
      Boolv = and(Leftv, Rightv)
  ; Bool = or(Left, Right), !,
      validateBool(Left, Names, Leftv),
      validateBool(Right, Names, Rightv),
      Boolv = or(Leftv, Rightv)
  ; Bool =.. [Op, Left, Right], !,
      validateExpr(Left, Names, Leftv),
      validateExpr(Right, Names, Rightv),
      Boolv =.. [Op, Leftv, Rightv]
  ).

validateProcCall(procCall(Id, Params), Names, procCall(Addr, Paramsv)) :-
  length(Params, ParamsC),
  validateProcId(Id, ParamsC, Names, Args, Addr), !,
  validateActualParams(Params, Args, Names, Paramsv).

validateProcId(Id, ParamsC, [N| _], Args, NewAddr) :-
  ( N = proc(Id, NewAddr, Args), !, length(Args, ParamsC)
  ; N = par(Id, Addr, Args), length(Args, ParamsC), NewAddr = env(Addr)
  ), !.
validateProcId(Id, ParamsC, [N| Names], Args, NewAddr) :-
  ( N = par(_, _, _), !, NewAddr = env(RetAddr)
  ; NewAddr = RetAddr
  ),
  validateProcId(Id, ParamsC, Names, Args, RetAddr).

validateActualParams([], _, _, []).
validateActualParams([H| T], [A|Args], Names, [Hv| Tv]) :-
  validateExpr(H, Names, Expv),
  ( A = name(_, Type), !,
    Hv = name(Expv, Type)
  ; A = value(_),
    Hv = value(Expv)),
  validateActualParams(T, Args, Names, Tv).

validateVar(variable(Id), [N| _Names], V) :-
  ( N = local(Id, Addr), !, V = variable(Addr)
  ; N = name(Id, Addr, Type), !, V = name(Addr, Type)
  ; N = value(Id, Addr), V = variable(Addr)
  ).
validateVar(variable(Id), [N| Names], Vv) :-
  validateVar(variable(Id), Names, V), !,
  ( N = par(_, _, _), !,
      NewAddr = env(Vaddr),
      ( V = name(Vaddr, Type),
        Vv = name(NewAddr, Type)
      ; V = variable(Vaddr),
        Vv = variable(NewAddr)
      )
  ; Vv = V
  ).

% deduceTypes works in two modes, first if given Flag as a free variable
% it will try to find which call by name parameters should be passed as a thunk,
% and which as a reference.
% In second mode, given Flag = add_references it will set all free Type variables
% to reference

deduceTypes(List) :-                  % iteratively resolves Type variables
  deduceTypes(List, Flag),
  ( var(Flag), !,
    deduceTypes(List, add_references)
  ; Flag = resolved,
    deduceTypes(List)
  ).

deduceTypes( [], _).
deduceTypes([H | T], Flag) :-
  H =.. [_, _, _Args, _Locals, Instr ],
  deduceTypesInstructions(Instr, Flag),
  deduceTypes(T, Flag).

deduceTypesProcCall(procCall(_Addr, Params), Flag) :-
  deduceTypesParams(Params, Flag).

deduceTypesParams([], _).
deduceTypesParams([P | Params], Flag) :-
  ( P = value(_Addr), !
  ; P = name(E, Type),
    ( E = variable(_Addr), !
    ; E = name(_Addr, Type), !,
      ( var(Type) -> Type = Type
      ; ( ground(Flag) -> ( Flag = resolved, !
                          ; Flag = add_references )
        ; true
        )
      )
    ; (var(Type) ->
        Type = thunk,
        deduceTypesExpr(E, Flag),
        Flag = resolved
      ; Type = thunk
      )
    )
  ), deduceTypesParams(Params, Flag).

deduceTypesInstructions([],  _).
deduceTypesInstructions([H|T], Flag) :-
  deduceTypesInstruction(H, Flag),
  deduceTypesInstructions(T, Flag).

deduceTypesInstruction(I, Flag) :-
  ( I = if(Bool, Then, Else), !,
      deduceTypesBool(Bool, Flag),
      deduceTypesInstructions(Then, Flag),
      deduceTypesInstructions(Else, Flag)
  ; I = if(Bool, Then), !,
      deduceTypesBool(Bool, Flag),
      deduceTypesInstructions(Then, Flag)
  ; I = while(Bool, Ins), !,
      deduceTypesBool(Bool, Flag),
      deduceTypesInstructions(Ins, Flag)
  ; I = discardReturn(Call), !,
      deduceTypesProcCall(Call, Flag)
  ; I = return(Exp), !,
      deduceTypesExpr(Exp, Flag)
  ; I = ioRead(V), !,
    ( V = name(_Addr, Type), !,
      ( var(Type) -> Type = reference, Flag = resolved
      ; Type = reference )
    ; V = V
    )
  ; I = ioWrite(Exp), !,
      deduceTypesExpr(Exp, Flag)
  ; I = assgn(V, Exp),
      deduceTypesExpr(Exp, Flag),
      ( V = name(_Addr, Type), !,
        ( var(Type) -> Type = reference, Flag = resolved
        ; Type = reference )
      ; V = V
      )
  ).

deduceTypesExpr(Expr, Flag) :-
  ( Expr = const(_), !
  ; Expr = variable(_Addr), !
  ; Expr = name(_Addr, Type), !,
      ( var(Type), ground(Flag), Flag = add_references ->
        Type = reference
      ; true
      )
  ; Expr = procCall(_Addr, _Params), !,
      deduceTypesProcCall(Expr, Flag)
  ; Expr = neg(E), !,
      deduceTypesExpr(E, Flag)
  ; Expr =.. [_Op, Left, Right], !,
      deduceTypesExpr(Left, Flag),
      deduceTypesExpr(Right, Flag)
  ).

deduceTypesBool(Bool, Flag) :-
  ( Bool = not(B), !,
      deduceTypesBool(B, Flag)
  ; Bool = and(Left, Right), !,
      deduceTypesBool(Left, Flag),
      deduceTypesBool(Right, Flag)
  ; Bool = or(Left, Right), !,
      deduceTypesBool(Left, Flag),
      deduceTypesBool(Right, Flag)
  ; Bool =.. [_Op, Left, Right], !,
      deduceTypesExpr(Left, Flag),
      deduceTypesExpr(Right, Flag)
  ).

% generate thunks from call by name parameters
% in following functions the variables with suffix cleaned will be used in
% normal procedures, and those with suffix Thunk in thunks

generateThunks(Procedures, All) :-
  deduceTypes(Procedures),                    % first deduce the types of call by name parameters
  phrase(generateThunks(Procedures), All).    % then generate neccessary thunks

generateThunks([]) --> [].
generateThunks([proc(Addr, Args, Locals, Instructions) | T]) -->
  { length(Args, ArgsC), length(Locals, LocalC) },
  [ proc(Addr, ArgsC, LocalC, InstructionsCleaned)],
  generateThunksInstructions(Instructions, InstructionsCleaned),
  generateThunks(T).

generateThunksProcCall(procCall(Addr, Params), procCall(env(Addr), ParamsThunk),
                       procCall(Addr, ParamsClean)) -->
  generateThunksParams(Params, ParamsThunk, ParamsClean).

generateThunksParams([], [], []) --> [].
generateThunksParams([H | T], [HT | TT], [HC | TC]) -->
  ( { H = value(Exp) }, !,
    generateThunksExpr(Exp, ExpThunk, ExpClean),
    { HT = value(ExpThunk),
      HC = value(ExpClean) }
  ; { H = name(V, reference) }, !,
    generateThunksExpr(V, VThunk, VClean),
    { HT = reference(VThunk),
      HC = reference(VClean) }
  ; { H = name(Exp, thunk) }, !,
    generateThunksExpr(Exp, ExpThunk, _ExpClean),
    [ proc(Addr, 0, 0, [ return(ExpThunk) ]) ],
    { HT = thunk(Addr),
      HC = thunk(Addr) }
  ),
  generateThunksParams(T, TT, TC).

generateThunksInstructions([], []) --> [].
generateThunksInstructions([H | T], [HC | TC]) -->
  ( { H = if(Bool, Then, Else) }, !,
      generateThunksBool(Bool, BoolClean),
      generateThunksInstructions(Then, ThenClean),
      generateThunksInstructions(Else, ElseClean),
      { HC = if(BoolClean, ThenClean, ElseClean) }
  ; { H = if(Bool, Then) }, !,
      generateThunksBool(Bool, BoolClean),
      generateThunksInstructions(Then, ThenClean),
      { HC = if(BoolClean, ThenClean) }
  ; { H = while(Bool, Ins) }, !,
      generateThunksBool(Bool, BoolClean),
      generateThunksInstructions(Ins, InstrClean),
      { HC = while(BoolClean, InstrClean) }
  ; { H = discardReturn(Call) }, !,
      generateThunksProcCall(Call, _, CallClean),
      { HC = discardReturn(CallClean) }
  ; { H = return(Exp) }, !,
      generateThunksExpr(Exp, _, ExpClean),
      { HC = return(ExpClean) }
  ; { H = ioRead(V), !, HC = ioRead(VC),
      ( V = name(Addr, reference), !, VC = reference(Addr)
      ; V = VC ),
      HC = ioRead(VC) }
  ; { H = ioWrite(Exp) }, !,
      generateThunksExpr(Exp, _, ExpClean),
      { HC = ioWrite(ExpClean) }
  ; { H = assgn(V, Exp) },
      generateThunksExpr(Exp, _, ExpClean),
      { ( V = name(Addr, reference), !, VC = reference(Addr)
        ; V = VC ),
        HC = assgn(VC, ExpClean) }
  ),
  generateThunksInstructions(T, TC).

  generateThunksExpr(Expr, ThunkExpression, CleanedExpression) -->
    ( { Expr = const(_), !,
        ThunkExpression = Expr,
        CleanedExpression = Expr }
    ; { Expr = variable(Addr), !,
        ThunkExpression = variable(env(Addr)),
        CleanedExpression = Expr }
    ; { Expr = name(Addr, Type), !,
        ThunkExpression =.. [ Type, env(Addr) ],
        CleanedExpression =.. [ Type, Addr ] }
    ; { Expr = procCall(_Addr, _Params) }, !,
      generateThunksProcCall(Expr, ThunkExpression, CleanedExpression)
    ; { Expr = neg(E) }, !,
        generateThunksExpr(E, EThunk, EClean),
      { ThunkExpression = neg(EThunk),
        CleanedExpression = neg(EClean) }
    ; { Expr =.. [Op, Left, Right] }, !,
        generateThunksExpr(Left, LeftThunk, LeftClean),
        generateThunksExpr(Right, RightThunk, RightClean),
      { ThunkExpression =.. [ Op, LeftThunk, RightThunk ],
        CleanedExpression =.. [ Op, LeftClean, RightClean ] }
    ).

  generateThunksBool(Bool, BoolClean) -->
    ( { Bool = not(B) }, !,
        generateThunksBool(B, BClean),
        { BoolClean = not(BClean) }
    ; { Bool = and(Left, Right) }, !,
        generateThunksBool(Left, LeftClean),
        generateThunksBool(Right, RightClean),
        { BoolClean = and(LeftClean, RightClean) }
    ; { Bool = or(Left, Right) }, !,
        generateThunksBool(Left, LeftClean),
        generateThunksBool(Right, RightClean),
        { BoolClean = or(LeftClean, RightClean) }
    ; { Bool =.. [ Op, Left, Right ] }, !,
        generateThunksExpr(Left, _, LeftClean),
        generateThunksExpr(Right, _, RightClean),
        { BoolClean =.. [ Op, LeftClean, RightClean ] }
    ).

 % generate assembly
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

 % assemble

assemble(Asm, Words) :-
  phrase(assemble(Asm, 0, [], 0, 0), Words).

assemble([], Acc, Const, LocalCnt, _ProgramCnt) -->
  { LC is LocalCnt + 1, pad(Acc, LC, NewAcc) },
  [ NewAcc ], Const.

assemble([H|T], Acc, Const, LocalCnt, ProgramCnt) -->
  ( { H = nop,      !, NewAcc is Acc,       LC is LocalCnt + 1, NewConst = Const }
  ; { H = syscall,  !, NewAcc is Acc + 1,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = load,     !, NewAcc is Acc + 2,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = store,    !, NewAcc is Acc + 3,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = swapa,    !, NewAcc is Acc + 4,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = swapd,    !, NewAcc is Acc + 5,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = branchz,  !, NewAcc is Acc + 6,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = branchn,  !, NewAcc is Acc + 7,   LC is LocalCnt + 1, NewConst = Const }
  ; { H = jump,     !, TmpAcc is Acc + 8,
      TC is LocalCnt + 1,
      pad(TmpAcc, TC, NewAcc),              LC is 4, NewConst = Const
    }
  ; { H = const(V), !, NewAcc is Acc + 9,
      append(Const, [V], NewConst),         LC is LocalCnt + 1 }
  ; { H = add,      !, NewAcc is Acc + 10,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = sub,      !, NewAcc is Acc + 11,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = mul,      !, NewAcc is Acc + 12,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = div,      !, NewAcc is Acc + 13,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = shift,    !, NewAcc is Acc + 14,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = nand,     !, NewAcc is Acc + 15,  LC is LocalCnt + 1, NewConst = Const }
  ; { H = label(Addr), !,
      ( LocalCnt = 0, !,
        Addr = PC,
        LC is LocalCnt,
        NewAcc is Acc
      ; TC is LocalCnt + 1,
        pad(Acc, TC, NewAcc),
        LC is 4,
        Addr = PC
      ), NewConst = Const
    }
  ), !,
  ( { LC = 4 }, !,
    [ NewAcc ], NewConst,
    { RC is 0, length(NewConst, CC),
      PC is ProgramCnt + CC + 1,
      A is 0,
      C = []
    }
  ; { RC is LC,
      PC is ProgramCnt,
      A is NewAcc * 16,
      C = NewConst
    }
  ), assemble(T, A, C, RC, PC).
  
pad(Acc, 4, Acc).
pad(Acc, Lc, Ret) :-
  NewAcc is 16 * Acc,
  NewLc is Lc + 1,
  pad(NewAcc, NewLc, Ret).
