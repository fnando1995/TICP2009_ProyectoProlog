# Proyecto con Prolog

## Primer entregable:

Dada una secuencia aritmetica con operadores (+,-,*,/) y agrupaciones (parentesis) entregar si la secuencia esta correcta o no.

Ej:

- Es correcta:  x=((4+5)-6)/-8
- No correcta:  x=(4+5))-6)/-8    # falta paréntesis

El programa debera entrega de resultado SI o NO dependiendo de la secuencia aritmetica.

### Solución

#### Assign Statement

Esta función compara si lo devuelto es []. La entrada es un string tokenizado.

Ej:  "x = (3+4)" ==> [x,=,(,3,+,4,)]

Esta cadena es separada y analizada según:

```
<assignStmt> --> <id> = <expr>
<expr> --> <expr> <operador1> <expr1> | <expr1>
<expr1> --> <expr1> <operador2> <expr2> | <expr2>
<expr2> --> <operador1> <expr> | <id> | <entero> | <numDecimal> | <string> | (<expr>)
<op2> --> * | /
<op1> --> + | -
```

descripción: 

La sentencia de asignación debe tener una variable seguida de un signo de 
asignación (en python = ). Lo restante del lado derecho es la expresión que 
se asignará. Se debe realizar en análisis de esta expresión.

La expresión se analiza separando partes de la lista sobrante según operadores
(+,-,\*,\/) y revisando a modo de arbol si esta sublista tiene un final de
tipo: operador1 (esto se usa para casos donde un operador2 está alado de un 
operador uno, ej: 3\*-4) junto a una expresión, id (variable), integer, float, 
String, expresion entre paréntesis.

Para separar el arbol de decisión, se utilizan los intermedios expr1 y expr2 
que limitan la expresión según un operador como se mencionó anteriormente.



### Ejecución: 


Dentro de `testcases/` se tienen varios `testX.txt`. Los test y resultados
esterados se pueden observar en `testcases/Review.txt`.

Para ejecutar cada aso:

```
swipl proj.pl
? executeProgram('testcases/testX.txt').
```

La función ejecuta la lectura del archivo completo y lo almacena en una
variable para luego tokenizarla y verificar si es una sentencia de 
asignación.