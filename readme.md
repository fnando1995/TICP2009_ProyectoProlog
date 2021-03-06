# Proyecto con Prolog

## Descripción

Como segunda entrega de proyecto se define realizar un programa en prolog capaz de leer la sintaxis de un archivo y entregar true o false dependiendo si es correcta o no la sintaxis usada.

El lenguaje escogido es C


## BNF
```
<asigop>            -->     =
<op1>               -->     - | +
<op2>               -->     * | /
<comop>             -->     == | < | > | <= | >= | <>  
<ops>               -->     <asigop> | <op1> | <op2> | <comop>
<var_id>            -->     <atom>\+<ops>
<array_id>          -->     <atom> [ <int> ]
<varlist>           -->     <varlistint> | <varlistfloat> | <varliststring>.
<varlistint>        -->     <int>,<varlistint> | <int>.
<varlistfloat>      -->     <float>,<varlistfloat> | <float>.
<varliststring>     -->     <string>,<varliststring> | <string>.
<id>                -->     <var_id> | <array_id> 
<idList>            -->     <id> | <id><idList>
<vartype>           -->     int | long | float | double | char | bool | void.
<expr2>             -->     <id> | <integer> | <float> | <stringLiteral> | (<expr>) 
<expr1>             -->     <expr1><op2><expr2> | <expr2>
<expr>              -->     <expr><op1><expr1> | <expr1>
<declareStmt>       -->     <vartype><idList>; | <vartype><assigStmt> | <vartype><assignArrayStmt>
<assignArrayStmt>   -->     <array_id><asigop>{<varlist>};
<assignStmt>        -->     <id><asigop><expr>;
<condExpr>          -->     <exp><compop><exp>
<ifStmt>            -->     if(<condExpr>){<listStmt>}else{<listStmt>} | if(<condExpr>){<listStmt>}
<whileStmnt>        -->     while(<condExpr>){<listStmt>}
<doWhileStmnt>      -->     do{<listStmt>}while(<condExpr>);
<functionStmt>      -->     <vartype><atom>(<id_list>) {<listStmt>}
<updateStmt>        -->     <var_id> = <expr> | <var_id><op1><op1> | <op1><op1><var_id>
<forStmt>           -->     for (<declareStmt> <condExpr>; updateStmt){<listStmt>} | for (<assignStmt> <condExpr>; updateStmt){<listStmt>}
<stmt>              -->     <declareStmt> | <assignStmt> | <ifStmt> | <whileStmt> | <doWhileStmt> | <functionStmt>
<listStmt>          -->     <stmt><listStmt> | <stmt>
<program>           -->     <listStmt>
```

## Ejecución: 


Dentro de `testcases2/` se tienen varios `testX.txt`. 

Para ejecutar cada caso:

```
swipl proj.pl
? executeProgram('testcases2/testX.txt').
```

La función ejecuta la lectura del archivo completo y lo almacena en una variable para luego tokenizarla y verificar si la sintaxis de todas las sentencias del programa son correctas.


## Consideraciones

Se debe de tomar en cuentas las siguientes consideraciones sobre la ejecución de este proyecto, pues no es 100% similar a un lector de sintaxis de C completo.

1.- Se obvian ingreso de librerias.

2.- Los arreglos se mantienen de una sola dimensión.

3.- Algunos errores tardarán en ser detectados dado que las sentencias llaman a otras sentencias anidadas, y debido a la recursión de fuerza bruta que realiza prolog, scripts largos pueden llegar a tardarse en culminar (entregando un false) por intentar todas las posibles combinaciones.

4.- La sentencia for, dentro de su parentesis entre la primera y la segunda expresión no valida que se tenga el ";" dado que este ya es validado por la sentence de declaración y asignación.

5.- La sentencia for podría agregar n sentencias en cada una de sus expresiones separadas por los ";", esto no fue agregado.











