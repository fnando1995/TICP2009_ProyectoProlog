%% Author: Javier Tibau
%% Tokenizer code
%% Created by Bruno Dufour, Fall 2005
%% Append
%% modified by Emmanuel Moran 2021 MCC

% no entiendo todavia ----------------------------------------
list([H|T],H,T).
% no entiendo todavia ----------------------------------------
append([ ],A,A).
append([A|B],C,[A|D]) :- append(B,C,D).

% gather es una funcion creada, el  simbolo --> no lo reconozco
gather(Chars) --> [C],  {alphaNumeric(C)}, gather(Rest), {Chars=[C|Rest]}.
gather([]) --> {true}.


% si C cumple las condicinoes entonces es alphanumerico
alphaNumeric(C):- 96<C,C<123;  % ascii letras minusculas
                  64<C, C<91;  % ascii letras mayusculas
                  47<C, C<58.  % ascii letras digitos
% - Floats ---------------------------------------------------------------------
digit(D) --> [D], {47 < D, D < 58}. % ascii letras digitos
nzdigit(D) --> [D], {48 < D, D < 58}.% ascii letras nonzero digitos
nedigits([D|T]) -->
        digit(D), 
        !,              % WTF?
        digits(T).
digits(L) --> nedigits(L).
digits([]) --> [].
floatlit(F) -->
        nzdigit(D0),
        digits(D1),
        ".",
        nedigits(D2),
        {append([D0|D1], [46], T), append(T, D2, D), name(F, D)}.
        % ascii 46 '.'

% - Strings --------------------------------------------------------------------
quote('"').
gatherString(Chars) --> [C], {C=\=34}, gatherString(Rest), {Chars=[C|Rest]}.
gatherString([]) --> {true}.
stringlit(S) --> "\"", gatherString(Chars), "\"", {string_to_list(S,Chars)}.

%operadores niveles 1 y 2
operador1(+).
operador1(-).
operador2(*).
operador2(/).
% Tokenize comparison operators
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
% Discard whitespace
tokenize(R)             -->[C],{C<33},tokenize(R).
% Tokenize special character
tokenize([N|R])         --> [C],{C>32}, {name(N,[C])},tokenize(R).
tokenize([])            -->[].



















%% <assignStmt> -->  <id> = <expr>
assignStmt([ID,=|TokenizedString]):-  
        % separdo el TokenizedString por '=' donde:
        % ID = antes del signo
        % TokenizedString lista luego del signo
        atom(ID),
        writeln(ID),
        writeln(TokenizedString).

%% Ejecucion:
%% Leo archivo txt, tokenizo y ejecuto program(en esta entrega saber si es un statement de asignacion)
parseTree(FileName):- 
        % se abre un archivo y el read completo se guarda en PRogramString
        open(FileName, 'read', InputStream),
        read_stream_to_codes(InputStream, ProgramString),
        close(InputStream),
        write('INPUT STREAM:'),
        writeln(ProgramString),
        % tokenize se resuelve con el stream del archivo en ProgramString en TS (TokenizedString)
        phrase(tokenize(TokenizedString), ProgramString),
        assignStmt(TokenizedString).