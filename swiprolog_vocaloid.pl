
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
    