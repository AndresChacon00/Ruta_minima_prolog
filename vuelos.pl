vuelo(pto_ordaz,maiquetia,7,8,5600,1,['l','m','mi','j','v','s','d']).
vuelo(maiquetia,porlamar,9,10,4800,2,['l','m','mi','j','v']).
vuelo(maiquetia,pto_ordaz,15,16,5300,3,['l','m','v']).
vuelo(pto_ordaz,maracaibo,13,15,9000,4,['l','m','mi','j','v','s','d']).
vuelo(maracaibo,maiquetia,17,18,5500,5,['l','m','mi','j','v','s','d']).
vuelo(pto_ordaz,valencia,11,12.5,6700,6,['l','m','mi','j','v','s','d']).
vuelo(maracaibo,valencia,8,8.75,4700,7,['l','m','mi','j','v','s','d']).
vuelo(porlamar,maracaibo,2,3.5,5900,8,['l','m','mi','j','v','s','d']).
vuelo(valencia,porlamar,17,18,4300,9,['l','s','d']).

vuelos_desde_ciudad(Ciudad,Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia):-
    vuelo(Ciudad,Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia).

append([],L2,L2).
append([H|T],L2,[H|L3]):-append(T,L2,L3).

rutas(Origen, Destino, HoraActual, CostoT, [(Destino, NuevoCostoT)]) :-
    vuelo(Origen, Destino, HoraSalida, _, Costo, _, _),
    HoraActual =< HoraSalida,
    NuevoCostoT is CostoT + Costo.

rutas(Origen, Destino, HoraActual, CostoT, [(Escala, NuevoCostoT)|RestoDestinos]) :-
    vuelo(Origen, Escala, HoraSalida, HoraLlegada, Costo, _, _),
    Escala \= Destino,
    HoraActual =< HoraSalida,
    NuevoHoraActual is HoraLlegada,
    NuevoCostoT is CostoT + Costo,
    rutas(Escala, Destino, NuevoHoraActual, NuevoCostoT, RestoDestinos).
