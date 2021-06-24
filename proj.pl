%% Author: Javier Tibau
%% Tokenizer code
%% Created by Bruno Dufour, Fall 2005
%% Append
%% modified by Emmanuel Moran 2021 MCC
%% Assignment Statement confirmation


%% import tokenize.pl for tokenizer | Tokenizer
%% Abstracted ------------------------------------------------------------------
list([H|T],H,T).
append([ ],A,A).
append([A|B],C,[A|D]) :- append(B,C,D).
gather(Chars) --> [C],  {alphaNumeric(C)}, gather(Rest), {Chars=[C|Rest]}.
gather([]) --> {true}.
% si C cumple las condicinoes entonces es alphanumerico
alphaNumeric(C):- 96<C,C<123;  % ascii letras minusculas
                  64<C, C<91;  % ascii letras mayusculas
                  47<C, C<58.  % ascii letras digitos
% - Floats ---------------------------------------------------------------------
digit(D)        --> [D], {47 < D, D < 58}. % ascii letras digitos
nzdigit(D)      --> [D], {48 < D, D < 58}. % ascii letras nonzero digitos
nedigits([D|T]) --> digit(D), !,digits(T).
digits(L)       --> nedigits(L).
digits([])      --> [].
floatlit(F)     --> nzdigit(D0),digits(D1),".",nedigits(D2),{append([D0|D1], [46], T), append(T, D2, D), name(F, D)}.
% ascii 46 es '.'
% - Strings --------------------------------------------------------------------
quote('"').
gatherString(Chars)     --> [C], {C=\=34}, gatherString(Rest), {Chars=[C|Rest]}.
gatherString([])        --> {true}.
stringlit(S)            --> "\"", gatherString(Chars), "\"", {string_to_list(S,Chars)}.

% Tokenize comparison operators
% no usado en esta primera entrega
tokenize(Z)             --> "==", tokenize(Y), {Z = [== | Y]}.
tokenize(Z)             --> ">=", tokenize(Y), {Z = [>= | Y]}.
tokenize(Z)             --> "<=", tokenize(Y), {Z = [<= | Y]}.
tokenize(Z)             --> "<>", tokenize(Y), {Z = [<> | Y]}.
tokenize(Z)             --> ">",  tokenize(Y), {Z = [>  | Y]}.
tokenize(Z)             --> "<",  tokenize(Y), {Z = [<  | Y]}.
% Tokenize float
tokenize(Result)        --> floatlit(F), tokenize(Rest), {Result=[F|Rest]}.
% Tokenize string
tokenize(Result)        --> stringlit(S), tokenize(Rest), {Result=[S|Rest]}.
% Tokenize id / int
tokenize(Result)        --> gather(Chars),{\+ Chars =[]},tokenize(RestResult), {name(N,Chars), Result=[N|RestResult]}. 
% Discard whitespace   ASCII 32 is whitespace
tokenize(R)             -->[C],{C<33},tokenize(R).
% Tokenize special character ASCCI 32 is whitespace, upper tokenize rules validate some range of >32
tokenize([N|R])         --> [C],{C>32}, {name(N,[C])},tokenize(R).
% No more to do return
tokenize([])            -->[].

%% ---------------abstracted-------------
%% Tokenize("x=3+4") ==> [x,=,3,+,4]


%% <asigop> --> =
asigop(=).
%% <op1> --> - | +
op1(+).
op1(-).
%% <op2> --> * | /
op2(*).
op2(/).
%% <comop> --> == | < | > | <= | >= | <>
comop(==).
comop(>=).
comop(<=).
comop(>).
comop(<).
comop(<>).
%% <ops>
ops(X):- asigop(X) | op1(X) | op2(X) | comop(X).
%% <var_id> --> <atom> | \+ <ops>
var_id([ID|TSEnd],TSEnd):- atom(ID),\+ ops(ID).
var_id(ID,[]):- atom(ID),\+ ops(ID).
%% <array_id> --> <atom> [ <int> ]
array_id([X|['[',Y,']'|TSEnd]],TSEnd):- var_id(X,[]),integer(Y).
%% <varlist> --> <varlistint> | <varlistfloat> | <varliststring>.
%% <varlistint> --> <int>,<varlistint> | <int>.
%% <varlistfloat> --> <float>,<varlistfloat> | <float>.
%% <varliststring> --> <string>,<varliststring> | <string>.
varlist(TSInit,TSEnd) :- varlistint(TSInit,TSEnd) | varlistfloat(TSInit,TSEnd) | varliststring(TSInit,TSEnd).
varlistint([X,','|TSInit],TSEnd) :- integer(X),varlistint(TSInit,TSEnd).
varlistint([X|TSEnd],TSEnd) :-integer(X).
varlistfloat([X,','|TSInit],TSEnd) :- float(X),varlistfloat(TSInit,TSEnd).
varlistfloat([X|TSEnd],TSEnd) :-float(X).
varliststring([X,','|TSInit],TSEnd) :- string(X),varliststring(TSInit,TSEnd).
varliststring([X|TSEnd],TSEnd) :-string(X).
%% <id> -->  <var_id>| <array> 
id(TSInit,TSEnd):- var_id(TSInit,TSEnd) | array_id(TSInit,TSEnd). 
%% <idList> --> <id> | <id><idList>
idList(TSInit,TSEnd):- id(TSInit,TSEnd).
idList(TSInit,TSEnd):- id(TSInit,[','|TSInit_I]),idList(TSInit_I,TSEnd).
%% <vartype> --> int | long | float | double | char | bool.
vartype(int).
vartype(long).
vartype(float).
vartype(double).
vartype(char).
vartype(bool).
vartype(void).
%% <expr2> --> <id> | <integer> | <numWDecimal> | <stringLiteral> | (<expr>) 
expr2([X|TSEnd_I],TSEnd):-      op1(X),expr(TSEnd_I,TSEnd).
expr2([X|TSEnd],TSEnd):-        var_id(X,[]).
expr2([X|TSEnd],TSEnd):-        integer(X).
expr2([X|TSEnd],TSEnd):-        float(X).
expr2([X|TSEnd],TSEnd):-        string(X).
expr2(['('|TSInit], TSEnd ):-   expr(TSInit, [ ')' | TSEnd ]).
%% <expr1> --> <expr1> <op2> <expr2> | <expr2>
expr1(TSInit,TSEnd):- expr2(TSInit,[OP|TSEnd_I]), op2(OP), expr1(TSEnd_I,TSEnd).
expr1(TSInit,TSEnd):- expr2(TSInit,TSEnd).
%% <expr> --> <expr> <op1> <expr1> | <expr1>
expr(TSInit,TSEnd):- expr1(TSInit,[OP|TSEnd_I]), op1(OP), expr(TSEnd_I,TSEnd).
expr(TSInit,TSEnd):- expr1(TSInit,TSEnd).
%% <declareStmt> --> <vartype><idList>; | <vartype><assigStmt> | <vartype><assignArrayStmt>
declareStmt([VARTYPE|TSInit],TSEnd) :- vartype(VARTYPE),idList(TSInit,[';'|TSEnd]).
declareStmt([VARTYPE|TSInit],TSEnd) :- vartype(VARTYPE),assignStmt(TSInit,TSEnd).
declareStmt([VARTYPE|TSInit],TSEnd) :- vartype(VARTYPE),assignArrayStmt(TSInit,TSEnd).
%% <assignArrayStmt> -->  <array_id><asigop>{<varlist>};
assignArrayStmt(TSInit,TSEnd):- array_id(TSInit,[X,'{'|TSInit_I]),asigop(X),varlist(TSInit_I,['}',';'|TSEnd]).
%% <assignStmt> -->  <var_id> = <expr>;
assignStmt([ID,X|TSInit],TSEnd):-  var_id(ID,[]),asigop(X),expr(TSInit,[';'|TSEnd]).
%% <condExpr> --> <exp> <compop> <exp>
condExpr(TSInit,TSEnd):- expr(TSInit,[X|TSEnd_I]),comop(X),expr(TSEnd_I,TSEnd).  %%posible modif
%% <ifStmt> --> if (<condExpr>) {<listStmt>} else {<listStmt>}
ifStmt(['if','('|TSInit],TSEnd):- condExpr(TSInit,[')','{'|TSEnd_I]),listStmt(TSEnd_I,['}','else','{'|TSEnd_I1]),listStmt(TSEnd_I1,['}'|TSEnd]).
ifStmt(['if','('|TSInit],TSEnd):- condExpr(TSInit,[')','{'|TSEnd_I]),listStmt(TSEnd_I,['}'|TSEnd]).
%% <whileStmnt> --> while (<condExpr>) {<listStmt>}
whileStmt(['while','('|TSInit],TSEnd):- condExpr(TSInit,[')','{'|TSEnd_I]),listStmt(TSEnd_I,['}'|TSEnd]).
%% <doWhileStmnt> --> do{<listStmt>}while(<condExpr>);
doWhileStmt(['do','{'|TSInit],TSEnd):- listStmt(TSInit,['}','while','('|TSEnd_I]),condExpr(TSEnd_I,[')',';'|TSEnd]).
%% <function>--> <vartype> <atom>(<id_list>) {<listStmt>}
functionStmt([X,FUNCNAME,'('|TSInit],TSEnd):- vartype(X),atom(FUNCNAME),idList(TSInit,[')','{'|TSEnd_I]),listStmt(TSEnd_I,['}'|TSEnd]).
% <updateStmt> --> <var_id> = <expr> | <var_id><op1><op1> | <op1><op1><var_id>
updateStmt([ID,X|TSInit],TSEnd):- var_id(ID,[]),asigop(X),expr(TSInit,TSEnd).
updateStmt([ID,OP1,OP2|TSEnd],TSEnd):- var_id(ID,[]),op1(OP1),op1(OP2).
updateStmt([OP1,OP2,ID|TSEnd],TSEnd):- var_id(ID,[]),op1(OP1),op1(OP2).
%% <forStmt> --> for (<declareStmt> <condExpr>; updateStmt){<listStmt>} | for (<assignStmt> <condExpr>; updateStmt){<listStmt>}
forStmt(['for','('|TSInit],TSEnd):- declareStmt(TSInit,TSInit_I),condExpr(TSInit_I,[';'|TSInit_I2]),updateStmt(TSInit_I2,[')','{'|TSInit_I3]),listStmt(TSInit_I3,['}'|TSEnd]).
forStmt(['for','('|TSInit],TSEnd):- assignStmt(TSInit,TSInit_I),condExpr(TSInit_I,[';'|TSInit_I2]),updateStmt(TSInit_I2,[')','{'|TSInit_I3]),listStmt(TSInit_I3,['}'|TSEnd]).
%% <stmt> :- assignStmt | declareStatement
stmt(TSInit,TSEnd) :- forStmt(TSInit,TSEnd) | updateStmt(TSInit,[';'|TSEnd]) | declareStmt(TSInit,TSEnd) | assignStmt(TSInit,TSEnd) | ifStmt(TSInit,TSEnd) | whileStmt(TSInit,TSEnd) | doWhileStmt(TSInit,TSEnd) | functionStmt(TSInit,TSEnd).
listStmt(TSInit,TSEnd) :- stmt(TSInit,TSInit_I),listStmt(TSInit_I,TSEnd) | stmt(TSInit,TSEnd).
program(TSInit,TSEnd):- listStmt(TSInit,TSEnd).
%% Ejecucion:
%% Leo archivo txt, tokenizo y ejecuto program(en esta entrega saber si es un statement de asignacion)
executeProgram(FileName):- 
        % se abre un archivo y el read completo se guarda en PRogramString.
        open(FileName, 'read', InputStream),
        read_stream_to_codes(InputStream, ProgramString),
        close(InputStream),
        %% tokenize se resuelve con el stream del archivo en ProgramString en TSInit.
        phrase(tokenize(TSInit), ProgramString),
        %% write('TSInit:'),writeln(TSInit),
        %% verifico si TSinit es un assign Statement.
        program(TSInit,[]).



