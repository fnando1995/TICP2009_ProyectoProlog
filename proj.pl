%% Author: Javier Tibau
%% Tokenizer code
%% Created by Bruno Dufour, Fall 2005
%% Append
%% modified by Emmanuel Moran 2021 MCC
%% Assignment Statement confirmation




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


/* 
Desarrollo: 

<op1> --> + | -
<op2> --> * | /
Se asigna operadores y se categoriza
segun su nivel de importancia en una secuencia
aritmetica normal. Primero + y -, luego * y /

<assignStmt> --> <id> = <expr>
Se intenta sabe si es un operador de asignacion
se separa la lista tokenizada en dos partes separadas por
el operador '='. A la izquierda <id> a la derecha <expr>
esta expresion se debe evaluar si esta correcta.

<expr> --> <expr> <op1> <expr1> | <expr1>
Se entiende (o define) como <expr> a una expresion
que puede tener operadores nivel 1 y 2. Una <expr>
puede ser considerado una <expr1> para intentar
resolver, caso contrario sera un false(BT)

<expr1> --> <expr1> <op2> <expr2> | <expr2>
Se entiende (o define) como <expr1> a una expresion
que puede tener operadores nivel 2. Una <expr1> 
puede ser considerado una <expr0> para intentar
resolver caso contrario sera un false.(BT)

<expr2> --> <id> | <entero> | <numDecimal> | (<expr>)
Se entiende (o define) como <expr2> a una expresion
que puede contener id,integer,float,string o (<expr>)
que es una expresion entre parentesis y se 
resuelve eliminando los parentesis (en pares) y resolviendo
lo interno (expr). En caso que no se resuelve con estos,
se entendra que la <expr0> es erronea por lo cual se
retornara un false.
 */

%operadores niveles 1 y 2
operador1(+).
operador1(-).
operador2(*).
operador2(/).
% se define id como un atom(). 
id(ID):- atom(ID).

%% <expr0> --> <id> | <integer> | <numWDecimal> | <stringLiteral> | (<expr>) 
expr2([X|TSEnd_I],TSEnd):-      operador1(X),expr(TSEnd_I,TSEnd).
expr2([X|TSEnd],TSEnd):-        id(X).
expr2([X|TSEnd],TSEnd):-        integer(X).
expr2([X|TSEnd],TSEnd):-        float(X).
expr2([X|TSEnd],TSEnd):-        string(X).
expr2(['('|TSInit], TSEnd ):-   expr(TSInit, [ ')' | TSEnd ]).

%% <expr1> --> <expr1> <op2> <expr2> | <expr2>
expr1(TSInit,TSEnd):- expr2(TSInit,[OP|TSEnd_I]), operador2(OP), expr1(TSEnd_I,TSEnd).
expr1(TSInit,TSEnd):- expr2(TSInit,TSEnd).

%% <expr> --> <expr> <op1> <expr1> | <expr1>
expr(TSInit,TSEnd):- expr1(TSInit,[OP|TSEnd_I]), operador1(OP), expr(TSEnd_I,TSEnd).
expr(TSInit,TSEnd):- expr1(TSInit,TSEnd).

%% <assignStmt> -->  <id> = <expr>
assignStmt([ID,=|TSInitNoID],TSEnd):-  
        % separo el TokenizedString por '=' donde:
        % ID = antes del signo
        % TSInitNoID lista luego del signo
        % TSEnd es el final, se compara con [] para verificar si el assignStmt es correcto.
        id(ID),                       % verifica si es atom(id)
        %write('AssignStmt'),writeln(TSInitNoID),            % debug imprimo TSInitNoID para continuar
        expr(TSInitNoID,TSEnd).

%% Ejecucion:
%% Leo archivo txt, tokenizo y ejecuto program(en esta entrega saber si es un statement de asignacion)
executeProgram(FileName):- 
        % se abre un archivo y el read completo se guarda en PRogramString.
        open(FileName, 'read', InputStream),
        read_stream_to_codes(InputStream, ProgramString),
        close(InputStream),
        %write('INPUT STREAM:'),writeln(ProgramString),
        % tokenize se resuelve con el stream del archivo en ProgramString en TSInit.
        phrase(tokenize(TSInit), ProgramString),
        %write('TSInit:'),writeln(TSInit),
        % verifico si TSinit es un assign Statement.
        assignStmt(TSInit,[]).




