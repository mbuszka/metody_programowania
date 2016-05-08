parseAtom(Text, AST) :-
  atom_codes(Text, Codes),
  lex(Codes, Tokens),
  parse(Tokens, AST).

parseString(Text, AST) :-
  string_codes(Text, Codes),
  lex(Codes, Tokens),
  parse(Tokens, AST).

parseFile(FileName, AST) :-
  open(FileName, read, Stream),
  read_string(Stream, _, Text),
  parseString(Text, AST).

compileAtom(Atom, Procedures, Thunks, Assembly) :-
  parseAtom(Atom, AST),
  validate(AST, Procedures),
  generateThunks(Procedures, Thunks),
  generateProgram(Thunks, Assembly),
  assemble(Assembly, Words),
  printHex(Words).

compileString(Text, Procedures, Thunks, Assembly) :-
  parseString(Text, AST),
  validate(AST, Procedures),
  generateThunks(Procedures, Thunks),
  generateProgram(Thunks, Assembly),
  assemble(Assembly, Words),
  printHex(Words).

compileFile(InFile, Procedures, Thunks, Assembly) :-
  parseFile(InFile, AST),
  validate(AST, Procedures),
  generateThunks(Procedures, Thunks),
  generateProgram(Thunks, Assembly),
  assemble(Assembly, Words),
  printHex(Words).

saveHex(Words, FileName) :-
  open(FileName, write, File),
  saveHex_(Words, File),
  close(File).

saveHex_([], _File).
saveHex_([H|T], File) :-
  format(File, '~|~`0t~16r~4+~n', H),
  saveHex_(T, File).

printHex([]).
printHex([H|T]) :-
  format('~|~`0t~16r~4+~n', H),
  printHex(T).
