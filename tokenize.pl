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