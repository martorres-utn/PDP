%Parte 1

ingresante(harry, sangreMestiza, [corajudx, amistosx, orgullosx, inteligente], slytherin).
ingresante(draco, sangrePura, [orgullosx, inteligente], hufflepuff).
ingresante(hermione, sangreImpura, [inteligente, orgullosx, responsable]).

casaPrioridad(gryffindor, [corajudx]).
casaPrioridad(slytherin, [orgullosx, inteligente]).
casaPrioridad(ravenclaw, [responsable, inteligente]).
casaPrioridad(hufflepuff, [amistosx]).

/*
1. Saber si una casa permite entrar a un mago, lo cual se cumple para cualquier mago y cualquier casa excepto en el caso de Slytherin, que no permite entrar a magos de sangre impura.
*/

casaPermiteEntrada(slytherin, Mago) :-
    ingresante(Mago, sangrePura, _, _).
casaPermiteEntrada(slytherin, Mago) :-
    ingresante(Mago, sangrePura, _).
casaPermiteEntrada(Casa, Mago) :- 
    ingresante(Mago, _, _, _),
    casaPrioridad(Casa, _),
    Casa \= slytherin.
casaPermiteEntrada(Casa, Mago) :- 
    ingresante(Mago, _, _),
    casaPrioridad(Casa, _),
    Casa \= slytherin.

/*
2. Saber si un mago tiene el carácter apropiado para una casa, lo cual se cumple para cualquier mago si sus características incluyen todo lo que se busca para los integrantes de esa casa, independientemente de si la casa le permite la entrada.
*/

caracterApropiado(Casa, Mago) :- 
    ingresante(Mago, _, Caracter, _), 
    casaPrioridad(Casa, Prioridades), 
    forall(member(Prioridad, Prioridades), member(Prioridad, Caracter)).
caracterApropiado(Casa, Mago) :- 
    ingresante(Mago, _, Caracter), 
    casaPrioridad(Casa, Prioridades), 
    forall(member(Prioridad, Prioridades), member(Prioridad, Caracter)).

/*
3. Determinar en qué casa podría quedar seleccionado un mago sabiendo que tiene que tener el carácter adecuado para la casa, la casa permite su entrada y además el mago no odiaría que lo manden a esa casa. Además Hermione puede quedar seleccionada en Gryffindor, porque al parecer encontró una forma de hackear al sombrero.
*/

posibleCasa(Casa, Mago) :-
    casaPermiteEntrada(Casa, Mago),
    caracterApropiado(Casa, Mago),
    not((ingresante(Mago, _, _, CasaOdiada), CasaOdiada = Casa)).
posibleCasa(gryffindor, hermione).

/*
4. Definir un predicado cadenaDeAmistades/1 que se cumple para una lista de magos si todos ellos se caracterizan por ser amistosos y cada uno podría estar en la misma casa que el siguiente. No hace falta que sea inversible, se consultará de forma individual.
*/

esAmistoso(Mago) :- ingresante(Mago, _, Caracter, _), member(amistosx, Caracter).
esAmistoso(Mago) :- ingresante(Mago, _, Caracter), member(amistosx, Caracter).

cadenaDeAmistades([]).
cadenaDeAmistades([Mago0, Mago1 | Cola]) :- 
    esAmistoso(Mago0), 
    esAmistoso(Mago1), 
    posibleCasa(Mago0, MismaCasa), 
    posibleCasa(Mago1, MismaCasa), 
    cadenaDeAmistades([Mago1 | Cola]).
