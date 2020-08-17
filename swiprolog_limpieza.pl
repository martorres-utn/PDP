herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(limpiarTecho, [escoba, pala]).
herramientasRequeridas(cortarPasto, [bordedadora]).
herramientasRequeridas(limpiarBanio, [sopapa, trapeador]).
herramientasRequeridas(encerarPisos, [lustradpesora, cera, aspiradora(300)]).

%1

tiene(egon, [aspiradora(200), trapeador]).
tiene(peter, [trapeador]).
tiene(winston, [varitaNeutrones]).

%2
satisfaceHerramienta(Persona, aspiradora(Potencia)) :-
    tiene(Persona, Inventario),
    member(aspiradora(CualquierPotencia), Inventario),
    CualquierPotencia >= Potencia.
satisfaceHerramienta(Persona, Herramienta) :-
    tiene(Persona, Inventario),
    member(Herramienta, Inventario).

%3
puedeRealizarTarea(Persona, Tarea) :- 
    herramientasRequeridas(Tarea, _), 
    satisfaceHerramienta(Persona, varitaNeutrones).
puedeRealizarTarea(Persona, Tarea) :-
    herramientasRequeridas(Tarea, HerramientasRequeridas),
    forall(member(Herramienta, HerramientasRequeridas), satisfaceHerramienta(Persona, Herramienta)).

%4
tareaPedida(juan, ordenarCuarto, 25).
tareaPedida(juan, encerarPisos, 25).
tareaPedida(ana, cortarPasto, 100).
tareaPedida(ana, limpiarTecho, 15).

precio(ordenarCuarto, 2).
precio(limpiarTecho, 3).
precio(cortarPasto, 4).
precio(limpiarBanio, 5).
precio(encerarPisos, 6).

facturaCliente(Cliente, Total) :-
    tareaPedida(Cliente, _, _),
    findall(TotalTarea, ( tareaPedida(Cliente, Tarea, MetrosCuadrados), costoPorMetro(Tarea, MetrosCuadrados, TotalTarea) ), TotalesPorTarea),
    sum_list(TotalesPorTarea, Total).

costoPorMetro(Tarea, MetrosCuadrados, CostoTotal) :-
    precio(Tarea, PrecioPorMetro),
    CostoTotal is MetrosCuadrados * PrecioPorMetro.