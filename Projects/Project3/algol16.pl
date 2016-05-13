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
