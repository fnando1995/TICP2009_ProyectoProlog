operador1(+).
operador1(-).
operador2(*).
operador2(/).

publicar([T]):- writeln(T).
verificar(TS) :- publicar([OP,operador1|TS]),operador1(OP).