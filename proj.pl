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



finlinea(';').
%% <coma>--> ','
coma(',').
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
%% <variable> --> <atom> | \+ <ops>
variable(ID):- atom(ID),\+ ops(ID).
%% <array_var> --> <atom>[<int>]
%array_var([ID,'[',INT,']']):- variable(ID),integer(INT).
%% <array> --> Definir un array ???
%% <>

%% <id> --> <array_id> | <variable>
id(X):- variable(X). %| array_var(X).

%% <idList> --> <id> | <id><idList>
idList([ID,';'|TSEnd],TSEnd):- id(ID).
idList([ID,','|TSInit],TSEnd):- id(ID),idList(TSInit,TSEnd).

%% <vartype> --> int | long | float | double | char | bool.
vartype(int).
vartype(long).
vartype(float).
vartype(double).
vartype(char).
vartype(bool).

%% <declarationStatement> --> <vartype><idList>;
declarationStatement([VARTYPE|TSInit],TSEnd) :- vartype(VARTYPE),idList(TSInit,TSEnd).



%% <expr0> --> <id> | <integer> | <numWDecimal> | <stringLiteral> | (<expr>) 
expr2([X|TSEnd_I],TSEnd):-      op1(X),expr(TSEnd_I,TSEnd).
expr2([X|TSEnd],TSEnd):-        variable(X).
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

%% <assignStmt> -->  <id> = <expr>
assignStmt([ID,X|TSInitNoID],TSEnd):-  variable(ID),asigop(X),expr(TSInitNoID,TSEnd).


program(TSInit,TSEnd):- assignStmt(TSInit,TSEnd) | declarationStatement(TSInit,TSEnd).


%% Ejecucion:
%% Leo archivo txt, tokenizo y ejecuto program(en esta entrega saber si es un statement de asignacion)
executeProgram(FileName):- 
        % se abre un archivo y el read completo se guarda en PRogramString.
        open(FileName, 'read', InputStream),
        read_stream_to_codes(InputStream, ProgramString),
        close(InputStream),
        %% tokenize se resuelve con el stream del archivo en ProgramString en TSInit.
        phrase(tokenize(TSInit), ProgramString),
        %write('TSInit:'),writeln(TSInit),
        %% verifico si TSinit es un assign Statement.
        program(TSInit,[]).




