
sabeCantar(megurineLuka, cancion(nightFever,4)).
sabeCantar(megurineLuka, cancion(foreverYoung,5)).
sabeCantar(hatsuneMiku, cancion(tellYourWorld, 4)).
sabeCantar(gumi, cancion(foreverYoung, 4)).
sabeCantar(gumi, cancion(tellYourWorld, 5)).
sabeCantar(seeU, cancion(novemberRain, 6)).
sabeCantar(seeU, cancion(nightFever, 5)).

cantante(Cantante) :-
    sabeCantar(Cantante, _).

esNovedoso(Cantante) :-
    cantante(Cantante),
    findall(Duracion, sabeCantar(Cantante, cancion(_, Duracion)), Duraciones),
    length(Duraciones, Cantidad),
    Cantidad >= 2,
    sum_list(Duraciones, Total),
    Total < 15.
    
cantanteAcelerado(Cantante) :-
    cantante(Cantante),
    not( (sabeCantar(Cantante, cancion(_, Duracion)), Duracion > 4) ).

concierto(mikuExpo, estadosUnidos, 2000, gigante(2, 6)).
concierto(magicalMirai, japon, 3000, gigante(3, 10)).
concierto(vocalektVisions, estadosUnidos, 1000, mediano(9)).
concierto(milkuFest, argentina, 100, pequeño(4)).

puedeParticipar(hatsuneMiku, _).
puedeParticipar(Cantante, Concierto) :-
    cantante(Cantante),
    concierto(Concierto, _, _, Tipo),
    cumpleRequisitos(Cantante, Tipo).

totalCanciones(Cantante, Cantidad, DuracionTotal) :-
    cantante(Cantante),
    findall(Duracion, sabeCantar(Cantante, cancion(_, Duracion)), Duraciones),
    length(Duraciones, Cantidad),
    sum_list(Duraciones, DuracionTotal).

cumpleRequisitos(Cantante, gigante(CantidadMinimaCanciones, DuracionMinimaConcierto)) :-
    totalCanciones(Cantante, Cantidad, DuracionTotal),
    Cantidad >= CantidadMinimaCanciones,
    DuracionTotal > DuracionMinimaConcierto.
cumpleRequisitos(Cantante, mediano(MaximaDuracionTotal)) :-
    totalCanciones(Cantante, _, DuracionTotal),
    DuracionTotal < MaximaDuracionTotal.
cumpleRequisitos(Cantante, pequeño(MinimaDuracionDeUnaCancion)) :-
    sabeCantar(Cantante, cancion(_, Duracion)),
    Duracion > MinimaDuracionDeUnaCancion.

totalFamaConcierto(Cantante, TotalFama) :-
    cantante(Cantante),
    findall(FamaConcierto, (puedeParticipar(Cantante, Concierto), concierto(Concierto, _, FamaConcierto, _)), NivelesDeFamaConcierto),
    sum_list(NivelesDeFamaConcierto, TotalFama).

nivelDeFama(Cantante, Nivel) :-
    totalFamaConcierto(Cantante, TotalFamaConcierto),
    totalCanciones(Cantante, CantidadCanciones, _),
    Nivel is TotalFamaConcierto * CantidadCanciones.

cantanteMasFamoso(Cantante) :-
    cantante(Cantante),
    nivelDeFama(Cantante, NivelMasAlto),
    forall(nivelDeFama(_, CualquierNivel), CualquierNivel =< NivelMasAlto).

seConocen(megurineLuka, hatsuneMiku).
seConocen(megurineLuka, gumi).
seConocen(gumi, seeU).
seConocen(seeU, kaito).

conocidoCercano(Cantante, OtroCantante) :- seConocen(Cantante, OtroCantante). 
conocidoCercano(Cantante, OtroCantante) :- seConocen(OtroCantante, Cantante).

conocido(Cantante, OtroCantante) :- conocidoCercano(Cantante, OtroCantante).
conocido(Cantante, OtroCantante) :- conocidoCercano(OtroCantante, Tercero), conocido(Cantante, Tercero).

unicoParticipante(Cantante, Concierto) :-
    puedeParticipar(Cantante, Concierto),
    not((conocido(Cantante, OtroCantante), puedeParticipar(OtroCantante, Concierto))).