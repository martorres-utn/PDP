/*Sueños
Un consorcio internacional nos pidió que relevemos su negocio, que consiste en hacer el seguimiento de los sueños que tiene cada una de las personas y los personajes que están destinados a cumplir esos sueños. 

Punto 1 

Queremos reflejar que 
Gabriel cree en Campanita, el Mago de Oz y Cavenaghi
Juan cree en el Conejo de Pascua
Macarena cree en los Reyes Magos, el Mago Capria y Campanita
Diego no cree en nadie

Conocemos tres tipos de sueño
ser un cantante y vender una cierta cantidad de “discos” (≅ bajadas)
ser un futbolista y jugar en algún equipo
ganar la lotería apostando una serie de números

Queremos reflejar entonces que
Gabriel quiere ganar la lotería apostando al 5 y al 9, y también quiere ser un futbolista de Arsenal
Juan quiere ser un cantante que venda 100.000 “discos”
Macarena no quiere ganar la lotería, sí ser cantante estilo “Eruca Sativa” y vender 10.000 discos

Generar la base de conocimientos inicial
Indicar qué conceptos entraron en juego para cada punto.
*/

%personaCree(gabriel, [campanita, magoDeOz, cavenaghi]).
%personaCree(juan, [conejoDePascua]).
%personaCree(macarena, [reyesMagos, magoCapria, campanita]).

cree(gabriel, campanita).
cree(gabriel, magoDeOz).
cree(gabriel, cavenaghi).

cree(juan, conejoDePascua).
cree(macarena, reyesMagos).
cree(macarena, magoCapria).
cree(macarena, campanita).

%como diego no cree en nada no se agrega a la base de conocimiento

/*
suenio(unaPersona, cantante(500000)).
suenio(otraPersona, futbolista(equipo)).
suenio(terceraPersona, ganarLoteria(numero)). 
*/

quiere(gabriel, ganarLoteria([5,9])).
quiere(gabriel, futbolista(arsenal)).
quiere(juan, cantante(100000)).
quiere(macarena, cantante(10000)).

/*
Punto 2 

Queremos saber si una persona es ambiciosa, 
esto ocurre cuando la suma de dificultades de los sueños es mayor a 20. 
La dificultad de cada sueño se calcula como
6 para ser un cantante que vende más de 500.000 ó 4 en caso contrario
ganar la lotería implica una dificultad de 10 * la cantidad de los números apostados
lograr ser un futbolista tiene una dificultad de 3 en equipo chico o 16 en caso contrario. Arsenal y Aldosivi son equipos chicos.
Puede agregar los predicados que sean necesarios. El predicado debe ser inversible para todos sus argumentos. 
Gabriel es ambicioso, porque quiere ganar a la lotería con 2 números (20 puntos de dificultad) y quiere ser futbolista de Arsenal (3 puntos) = 23 que es mayor a 20. En cambio Juan y Macarena tienen 4 puntos de dificultad (cantantes con menos de 500.000 discos)
*/

persona(Persona) :- cree(Persona, _).
persona(Persona) :- quiere(Persona, _).

esAmbiciosa(Persona):- cree(Persona, _), 
    dificultadTotalSuenios(Persona, TotalDificultad), 
    TotalDificultad > 20.

dificultadTotalSuenios(Persona, TotalDificultad):-
    findall(Dificultad, dificultadSuenio(Persona, Dificultad), 	
            ListaDificultades),
    sumlist(ListaDificultades, TotalDificultad).

dificultadSuenio(Persona, Dificultad):- quiere(Persona, Suenio), 
    nivelDificultad(Suenio, Dificultad).

nivelDificultad(ganarLoteria(ListaNumeros), Total):- length(ListaNumeros, Cantidad), 
    Total is Cantidad * 10.
nivelDificultad(cantante(Cantidad), 6):- Cantidad > 500000.
nivelDificultad(cantante(Cantidad), 4):- Cantidad =< 500000.
nivelDificultad(futbolista(Equipo), 3):- equipoChico(Equipo).
nivelDificultad(futbolista(Equipo), 16):- not(equipoChico(Equipo)).

equipoChico(arsenal).
equipoChico(aldosivi).

%tieneQuimica(campanita, Persona) :- cree(Persona, campanita), dificultadSuenio(Persona, Dificultad), Dificultad < 5.
%tieneQuimica(Personaje, Persona) :- cree(Persona, Personaje), sueniosPuros(Persona), not(esAmbiciosa(Persona)).

/*
Punto 3 
Queremos saber si un personaje tiene química con una persona. Esto se da
si la persona cree en el personaje y...
para Campanita, la persona debe tener al menos un sueño de dificultad menor a 5.
para el resto, 
todos los sueños deben ser puros (ser futbolista o cantante de menos de 200.000 discos)
y la persona no debe ser ambiciosa

No puede utilizar findall en este punto.
El predicado debe ser inversible para todos sus argumentos.
Campanita tiene química con Gabriel (porque tiene como sueño ser futbolista de Arsenal, que es un sueño de dificultad 3 - menor a 5), y los Reyes Magos, el Mago Capria y Campanita tienen química con Macarena porque no es ambiciosa.
*/

tieneQuimica(Personaje, Persona):- cree(Persona, Personaje), 
    								cumpleCondiciones(Persona, Personaje).

cumpleCondiciones(Persona, campanita):- quiere(Persona, Suenio), 
    			nivelDificultad(Suenio, Dificultad), Dificultad < 5.

cumpleCondiciones(Persona, Personaje):- Personaje \= campanita,
    todosLosSueniosSonPuros(Persona), not(esAmbiciosa(Persona)).
todosLosSueniosSonPuros(Persona):- forall(quiere(Persona, Suenio), esPuro(Suenio)).
esPuro(futbolista(_)).
esPuro(cantante(CantidadDeDiscos)):- CantidadDeDiscos < 500000.


/*
Punto 4
Sabemos que
Campanita es amiga de los Reyes Magos y del Conejo de Pascua
el Conejo de Pascua es amigo de Cavenaghi, entre otras amistades

Necesitamos definir si un personaje puede alegrar a una persona, esto ocurre
si una persona tiene algún sueño
el personaje tiene química con la persona y...
el personaje no está enfermo
o algún personaje de backup no está enfermo. Un personaje de backup es un amigo directo o indirecto del personaje principal

Debe evitar repetición de lógica.
El predicado debe ser totalmente inversible.
Debe considerar cualquier nivel de amistad posible (la solución debe ser general).
Suponiendo que Campanita, los Reyes Magos y el Conejo de Pascua están enfermos, 
el Mago Capria alegra a Macarena, ya que tiene química con ella y no está enfermo
Campanita alegra a Macarena; aunque está enferma es amiga del Conejo de Pascua, que aunque está enfermo es amigo de Cavenaghi que no está enfermo.
*/

personaje(Personaje):- cree(_, Personaje).

amiga(campanita, reyesMagos).
amiga(campanita, conejoDePascua).
amiga(cavenaghi, conejoDePascua).
sonAmigos(Amigo, OtroAmigo):- amiga(Amigo, OtroAmigo).
sonAmigos(Amigo, OtroAmigo):- amiga(OtroAmigo, Amigo).
sonAmigos(Amigo, OtroAmigo):- amiga(Amigo, AmigoIntermedio), 
    						sonAmigos(AmigoIntermedio, OtroAmigo).

puedeAlegrar(Personaje, Persona):- quiere(Persona, _), personaje(Personaje).
puedeAlegrar(Personaje, Persona):- tieneQuimica(Personaje, Persona), 
    							cumpleCondicionesPersonajeoBackup(Personaje).


cumpleCondicionesPersonajeoBackup(Personaje):- not(estaEnfermo(Personaje)).
cumpleCondicionesPersonajeoBackup(Personaje):- sonAmigos(Personaje, OtroPersonaje), 
    										not(estaEnfermo(OtroPersonaje)).

estaEnfermo(campanita).
estaEnfermo(reyesMagos).
estaEnfermo(conejoDePascua).



    