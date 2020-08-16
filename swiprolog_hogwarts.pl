%Parte 1

ingresante(harry, sangreMestiza, [corajudx, amistosx, orgullosx, inteligente], slytherin).
ingresante(draco, sangrePura, [orgullosx, inteligente], hufflepuff).
ingresante(hermione, sangreImpura, [inteligente, orgullosx, responsable]).
ingresante(pepita, sangreImpura, [inteligente, orgullosx, responsable, amistosx]).

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

cadenaDeAmistades([UnMago, OtroMago]) :- 
    esAmistoso(UnMago), 
    esAmistoso(OtroMago), 
    posibleCasa(MismaCasa, UnMago), 
    posibleCasa(MismaCasa, OtroMago).
cadenaDeAmistades([UnMago, OtroMago | _]) :- 
    esAmistoso(UnMago), 
    esAmistoso(OtroMago), 
    posibleCasa(MismaCasa, UnMago), 
    posibleCasa(MismaCasa, OtroMago),
    cadenaDeAmistades([OtroMago | _]).


%Parte 2
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

%malas acciones

accion(andarFueraDeCama, -50).
accion(visitar(bosque), -50).
accion(visitar(seccionRestringidaBiblioteca), -10).
accion(visitar(tercerPiso), -75).

%buenas acciones
accion(ganarPartidaAjedrezMagico, 50).
accion(salvarAmigosConIntelecto, 50).
accion(derrotarVoldemort, 60).

%acciones de alumnos
alumnoAccion(harry, andarFueraDeCama).
alumnoAccion(hermione, visitar(tercerPiso)).
alumnoAccion(hermione, visitar(seccionRestringidaBiblioteca)).
alumnoAccion(harry, visitar(tercerPiso)).
alumnoAccion(harry, visitar(bosque)).
alumnoAccion(draco, visitar(mazmorras)).
alumnoAccion(ron, ganarPartidaAjedrezMagico).
alumnoAccion(hermione, salvarAmigosConIntelecto).
alumnoAccion(harry, derrotarVoldemort).

/*
1.a. Saber si un mago es buen alumno, que se cumple si hizo alguna acción y ninguna de las cosas que hizo se considera una mala acción (que son aquellas que provocan un puntaje negativo).
1.b. Saber si una acción es recurrente, que se cumple si más de un mago hizo esa misma acción.
*/

esBuenAlumno(Mago) :-
    alumnoAccion(Mago, _),
    forall((alumnoAccion(Mago, CualquierAccion), accion(CualquierAccion, Puntaje)), Puntaje >= 0).

accionEsRecurrente(Accion) :-
    alumnoAccion(UnMago, Accion),
    alumnoAccion(OtroMago, Accion),
    UnMago \= OtroMago.

/*
2. Saber cuál es el puntaje total de una casa, que es la suma de los puntos obtenidos por sus miembros.
*/

casaPuntaje(Casa, Puntaje) :-
    findall(Puntaje, (esDe(Miembro, Casa), alumnoAccion(Miembro, Accion), accion(Accion, Puntaje)), PuntajesCasa),
    sum_list(PuntajesCasa, Puntaje).


/*
3. Saber cuál es la casa ganadora de la copa, que se verifica para aquella casa que haya obtenido una cantidad mayor de puntos que todas las otras.
*/
