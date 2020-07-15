ultimo([E], E).
ultimo([_|Cola], Ultimo):- ultimo(Cola,Ultimo).

%a
encolar(Elem, [], [Elem]).
encolar(Elem, [Cab|Cola], [Cab|Lista]):- encolar(Elem,Cola,Lista).

%b maximo(L, Max), relaciona una lista con el elemento más grande. Realizarlo con y sin recursividad.

maximofa(lista,Max) :- member(Max,Lista), 
    				forall(member(Elem,Lista), Max >= Elem).

maximo([A],A).
maximo([A,B], A):- A > B. 
maximo([A,B], B):- A < B.
maximo([Cabeza|Cola], Max):- maximo(Cola, Max), Max > Cabeza.

%c. unirSinRepeticiones([3,4,5],[9,4,10],[3,5,9,4,10]). Relaciona 2 listas con la lista
%que se obtiene de la unión de estas sin elementos repeticiones

%unirSinRepeticiones([A], [B], [A,B]).
unirSinRepeticiones([], Lista, Lista).
unirSinRepeticiones([Elem|Cola], OtraLista, Lista):- member(Elem, OtraLista),
    unirSinRepeticiones(Cola, OtraLista, Lista).
unirSinRepeticiones([Elem|Cola], OtraLista, [Elem|RestoLista]):- not(member(Elem,OtraLista)), 
    unirSinRepeticiones(Cola, OtraLista, RestoLista).

%d. inteseccion([3,4,5],[2,7,5],[5]). Relaciona la lista 1 con la lista 2 y la lista 3 que es
%la resultante

interseccion(Lista1, Lista2, ListaInter) :- findall(Elem, estaEn(Elem, Lista1, Lista2), ListaInter).
estaEn(Elem, Lista1, Lista2):- member(Elem, Lista1), member(   Elem,Lista2).

%e. esCreciente([3,4,5,7]). Conocer si todos los elementos de la lista están ordenados de
%menor a mayor.
esCreciente([]).
esCreciente([_]).
esCreciente([H1, H2|T]) :- H1 < H2, esCreciente([H2|T]).

sublistaMayoresA([], _ , []).
sublistaMayoresA([Cab|Cola], Elem, [Cab|Lista]) :- Cab > Elem, sublistaMayoresA(Cola,Elem,Lista).
sublistaMayoresA([_|Cola], Elem, Lista) :- sublistaMayoresA(Cola,Elem,Lista).

%reversa

reversa([],[]).
reversa([Cab|Cola], Reversa) :- reversa(Cola, ReversaCola),
    encolar(Cab, ReversaCola, Reversa).

%Explosión Combinatoria:
%Desarrollar el predicado entretenimientos/2, relaciona una cantidad de dinero con los
%entretenimientos posibles que puede realizar con dicha cantidad.
entretenimiento(cine).
entretenimiento(teatro).
entretenimiento(pool).
entretenimiento(parqueTematico).
costo(cine, 30).
costo(teatro, 40).
costo(pool, 15).
costo(parqueTematico, 50).

entretenimientos(Dinero, Sublista) :- conjuntoEntretenimiento(ConjuntoEntretenimientos),
    sublista(ConjuntoEntretenimientos, Dinero, Sublista).

conjuntoEntretenimiento(Lista):- findall(Entre, entretenimiento(Entre), Lista).

sublista([], _, []).
sublista([Entre|Cola], Dinero ,[Entre | Resto]):- costo(Entre, Monto), Monto =< Dinero, DineroRestante is Dinero - Monto, sublista(Cola, DineroRestante, Resto).
sublista([_|Cola], Dinero ,Lista):- sublista(Cola, Dinero, Lista).

%Práctica Ejercicio Integrador - Parcial - &quot;Afirmativo&quot;

%El nuevo ministro de seguridad quiere premiar a sus efectivos policiales mediante un novedoso
%sistema de puntuación. En primer lugar, contamos con información acerca de las tareas que
%realizan los agentes de la policía federal. Asumimos que cada agente puede realizar muchas
%tareas diferentes y que de todo agente se conoce al menos una de sus tareas.
%tarea(agente, tarea, ubicacion)
%tareas:
% ingerir(descripcion, tamaño, cantidad)
% apresar(malviviente, recompensa)
% asuntosInternos(agenteInvestigado)
% vigilar(listaDeNegocios)

tarea(vigilanteDelBarrio, ingerir(pizza, 1.5, 2),laBoca).
tarea(vigilanteDelBarrio, vigilar([pizzeria, heladeria]), barracas).
tarea(canaBoton, asuntosInternos(vigilanteDelBarrio), barracas).
tarea(sargentoGarcia, vigilar([pulperia, haciendaDeLaVega,
plaza]),puebloDeLosAngeles).
tarea(sargentoGarcia, ingerir(vino, 0.5, 5),puebloDeLosAngeles).
tarea(sargentoGarcia, apresar(elzorro, 100), puebloDeLosAngeles).
tarea(vega, apresar(neneCarrizo,50),avellaneda).
tarea(jefeSupremo, vigilar([congreso,casaRosada,tribunales]),laBoca).

%Las ubicaciones que existen son las siguientes:

ubicacion(puebloDeLosAngeles).
ubicacion(avellaneda).
ubicacion(barracas).
ubicacion(marDelPlata).
ubicacion(laBoca).
ubicacion(uqbar).

%Por último, se sabe quién es jefe de quién:
%jefe(jefe, subordinado)

jefe(jefeSupremo,vega ).
jefe(vega, vigilanteDelBarrio).
jefe(vega, canaBoton).
jefe(jefeSupremo,sargentoGarcia).

%1) Hacer el predicado frecuenta/2 que relaciona un agente con una ubicación en la que suele
%estar. Debe ser inversible.
%● Los agentes frecuentan las ubicaciones en las que realizan tareas
%● Todos los agentes frecuentan buenos aires.
%● Vega frecuenta quilmes.
%● Si un agente tiene como tarea vigilar un negocio de alfajores, frecuenta Mar del Plata.

frecuenta(Agente, Ubicacion):- tarea(Agente, _, Ubicacion).
frecuenta(Agente, buenosAires):- agente(Agente).
frecuenta(vega, quilmes).
frecuenta(Agente, marDelPlata):- tarea(Agente, vigilar(Negocios), _), 
    	member(negocioAlfajores, Negocios).

agente(Agente):- tarea(Agente, _, _).



             
             