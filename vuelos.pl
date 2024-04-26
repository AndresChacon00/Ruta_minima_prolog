/* Base de conocimientos de pruebas */
vuelo(pto_ordaz,maiquetia,7,8,5600,1,['l','m','mi','j','v','s','d']).
vuelo(maiquetia,porlamar,9,10,4800,2,['l','m','mi','j','v']).
vuelo(maiquetia,pto_ordaz,15,16,5300,3,['l','m','v']).
vuelo(pto_ordaz,maracaibo,13,15,9000,4,['l','m','mi','j','v','s','d']).
vuelo(maracaibo,maiquetia,17,18,5500,5,['l','m','mi','j','v','s','d']).
vuelo(pto_ordaz,valencia,11,12.5,6700,6,['l','m','mi','j','v','s','d']).
vuelo(maracaibo,valencia,8,8.75,4700,7,['l','m','mi','j','v','s','d']).
vuelo(porlamar,maracaibo,2,3.5,5900,8,['l','m','mi','j','v','s','d']).
vuelo(valencia,porlamar,17,18,4300,9,['l','s','d']).

/* Dada una Ciudad genere la información de todos los vuelos que parten de ella. */
print_vuelos_desde_ciudad([]).
print_vuelos_desde_ciudad([(Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia)|T]) :-
    format('Destino: ~w, HoraSalida: ~w, HoraLlegada: ~w, Costo: ~w, Codigo: ~w, Frecuencia: ~w~n', [Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia]),
    print_vuelos_desde_ciudad(T).

vuelos_desde_ciudad(Ciudad):-
    findall((Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia), vuelo(Ciudad,Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia), Resultado),
    print_vuelos_desde_ciudad(Resultado).

/* Dada una Ciudad genere la información de todos los vuelos que llegan a ella. */
print_vuelos_a_ciudad([]).
print_vuelos_a_ciudad([(Ciudad,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia)|T]) :-
    format('Ciudad: ~w, HoraSalida: ~w, HoraLlegada: ~w, Costo: ~w, Codigo: ~w, Frecuencia: ~w~n', [Ciudad,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia]),
    print_vuelos_a_ciudad(T).

vuelos_a_ciudad(Destino) :-
    findall((Ciudad,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia), vuelo(Ciudad,Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia), Resultado),
    print_vuelos_a_ciudad(Resultado).

/*
    Dada dos ciudades (origen y destino), determine todas las rutas entre ellas (directas
    o con escala) que puedan hacerse en un día, indicando las ciudades involucradas y
    los costos asociados a cada una de las rutas
*/
append([],L2,L2).
append([H|T],L2,[H|L3]):-append(T,L2,L3).

isElement(X, [X|_]).
isElement(X, [_|T]):- isElement(X,T).

rutas(Origen, Destino, Dia, HoraActual, CostoT, [(Destino, NuevoCostoT)]) :-
    vuelo(Origen, Destino, HoraSalida, _, Costo, _, VuelosSemanales),
    isElement(Dia, VuelosSemanales),
    HoraActual =< HoraSalida,
    NuevoCostoT is CostoT + Costo.

rutas(Origen, Destino, HoraActual, CostoT, [(Escala, NuevoCostoT)|RestoDestinos]) :-
    vuelo(Origen, Escala, HoraSalida, HoraLlegada, Costo, _, VuelosSemanales),
    isElement(Dia, VuelosSemanales),
    Escala \= Destino,
    HoraActual =< HoraSalida,
    NuevoHoraActual is HoraLlegada,
    NuevoCostoT is CostoT + Costo,
    rutas(Escala, Destino, NuevoHoraActual, NuevoCostoT, RestoDestinos).



