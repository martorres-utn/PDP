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
