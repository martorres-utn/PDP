
cotizacion(dolar, 8, 43).
cotizacion(lira, 7, 41).
cotizacion(dolar,9, 43.5).
cotizacion(lira, 9, 39).
cotizacion(dolar,10, 43.8).
cotizacion(lira, 10, 38).
cotizacion(patacon, 9, 200).
cotizacion(patacon, 10, 210).


variacion(Moneda, Hora, Cuanto):-
    cotizacion(Moneda, Hora, Monto), HoraAnterior is Hora -1,
    cotizacion(Moneda, HoraAnterior, OtroMonto),
    Cuanto is Monto - OtroMonto.

aCuantoCerro(Moneda, Monto):-cotizacion(Moneda, Hora, Monto),
    forall(cotizacion(Moneda, OtroHorario, _), 
           OtroHorario =< Hora).

transaccion(juanCarlos, 8, 1000, dolar).
transaccion(juanCarlos, 9, 1000, lira).
transaccion(ypf, 10, 100005349503495035930, dolar).

hicieronTransaccionConMásDeUnaMoneda(Persona):- transaccion(Persona, _, _, Moneda), 
    transaccion(Persona, _, _, OtraMoneda), Moneda \= OtraMoneda.

monedaQueNadieHizoTransacciones(Moneda):- moneda(Moneda), 
    not(transaccion(_, _, _, Moneda)).
    
moneda(Moneda):- cotizacion(Moneda, _, _).

transaccionGrande(Quien):- transaccion(Quien, _, Monto, _), Monto > 1000000.

totalPesosCambiados(Persona, TotalPesos):- transaccion(Persona, _, _, _), 
    findall(Pesos, transaccion(Persona, _, Pesos, _), ListaPesos), 
    sumlist(ListaPesos, TotalPesos). 












    