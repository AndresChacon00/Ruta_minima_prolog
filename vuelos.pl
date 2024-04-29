/*
    Leer archivo de vuelos
    - Se asume que el archivo viene en formato csv sin cabecera
    - Las horas se manejan en formato militar (ej. 7 am = 700, 2 pm = 1400)
    - Los días de vuelo se representan como una cadena en conjunto, donde m es martes y x es miércoles
    - Ejemplo de fila del archivo:
        pto_ordaz,maiquetia,700,800,5600,1,lmxjvsd
*/
linea_a_vuelo(Linea) :-
    split_string(Linea, ',', '', Valores),
    Valores = [Origen, Destino, HoraSalida, HoraLlegada, Costo, Vuelo, Dias],
    atom_string(OrigenAtm, Origen),
    atom_string(DestinoAtm, Destino),
    number_string(HoraSalidaNum, HoraSalida),
    number_string(HoraLlegadaNum, HoraLlegada),
    number_string(CostoNum, Costo),
    number_string(VueloNum, Vuelo),
    atom_string(DiasAtm, Dias),
    atom_chars(DiasAtm, ListaDias),
    assertz(vuelo(OrigenAtm, DestinoAtm, HoraSalidaNum, HoraLlegadaNum, CostoNum, VueloNum, ListaDias)).

leer_lineas(Stream, Lineas) :-
    read_line_to_string(Stream, Linea),
    (
        Linea == end_of_file
        -> Lineas = []
        ;   leer_lineas(Stream, Resto),
            Lineas = [Linea|Resto]
    ).

leer_vuelos(NombreArchivo) :-
    open(NombreArchivo, read, Stream),
    leer_lineas(Stream, Lineas),
    close(Stream),
    maplist(linea_a_vuelo, Lineas),
    print('Vuelos agregados con éxito').

/*
    Dada una Ciudad genere la información de todos los vuelos que parten de ella.
*/
print_vuelos_desde_ciudad([]).
print_vuelos_desde_ciudad([(Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia)|T]) :-
    format('Destino: ~w, HoraSalida: ~w, HoraLlegada: ~w, Costo: ~w, Codigo: ~w, Frecuencia: ~w~n', [Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia]),
    print_vuelos_desde_ciudad(T).

vuelos_desde_ciudad(Ciudad):-
    findall((Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia), vuelo(Ciudad,Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia), Resultado),
    print_vuelos_desde_ciudad(Resultado).

/*
    Dada una Ciudad genere la información de todos los vuelos que llegan a ella.
*/
print_vuelos_a_ciudad([]).
print_vuelos_a_ciudad([(Ciudad,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia)|T]) :-
    format('Ciudad: ~w, HoraSalida: ~w, HoraLlegada: ~w, Costo: ~w, Codigo: ~w, Frecuencia: ~w~n', [Ciudad,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia]),
    print_vuelos_a_ciudad(T).

vuelos_a_ciudad(Destino) :-
    findall((Ciudad,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia), vuelo(Ciudad,Destino,HoraSalida,HoraLlegada,Costo,Codigo,Frecuencia), Resultado),
    print_vuelos_a_ciudad(Resultado).

/*
    Funciones de apoyo para los siguientes requerimientos
*/
obtener_costo_ruta([(_, _, _, Costo)], Costo).
obtener_costo_ruta([_|RestoDestinos], CostoFin) :-
    obtener_costo_ruta(RestoDestinos, CostoFin).

obtener_hora_salida_ruta([(_, Salida, _, _)|_], SalidaFin) :-
    SalidaFin is Salida.

obtener_hora_llegada_ruta([(_, _, Llegada, _)], LlegadaFin) :-
    LlegadaFin is Llegada.
obtener_hora_llegada_ruta([_|Resto], LlegadaFin) :-
    obtener_hora_llegada_ruta(Resto, LlegadaFin).

/*
    Dada dos ciudades (origen y destino), determine todas las rutas entre ellas (directas
    o con escala) que puedan hacerse en un día, indicando las ciudades involucradas y
    los costos asociados a cada una de las rutas
*/
isElement(X, [X|_]).
isElement(X, [_|T]):- isElement(X,T).

ajustarTiempo(Tiempo, TiempoAjustado):-
    Horas is Tiempo div 100,
    Minutos is Tiempo mod 100,
    (Minutos >= 60 -> NuevoHoras is Horas + 1, NuevoMinutos is Minutos - 60; NuevoHoras is Horas, NuevoMinutos is Minutos),
    TiempoAjustado is NuevoHoras*100 + NuevoMinutos.

rutas(Origen, Destino, Dia, HoraActual, CostoT, _, [(Destino, Salida, Llegada, NuevoCostoT)]) :-
    vuelo(Origen, Destino, HoraSalida, HoraLlegada, Costo, _, VuelosSemanales),
    isElement(Dia, VuelosSemanales),
    HoraActual =< HoraSalida,
    Salida is HoraSalida,
    Llegada is HoraLlegada,
    NuevoCostoT is CostoT + Costo.

rutas(Origen, Destino, Dia, HoraActual, CostoT, TiempoMinimoConexion, [(Escala, Salida, Llegada, NuevoCostoT)|RestoDestinos]) :-
    vuelo(Origen, Escala, HoraSalida, HoraLlegada, Costo, _, VuelosSemanales),
    isElement(Dia, VuelosSemanales),
    Escala \= Destino,
    HoraMasConexion is HoraActual + TiempoMinimoConexion,
    ajustarTiempo(HoraMasConexion, HoraActualConConexion),   
    HoraActualConConexion =< HoraSalida,
    NuevoHoraActual is HoraLlegada,
    Salida is HoraSalida,
    Llegada is HoraLlegada,
    NuevoCostoT is CostoT + Costo,
    rutas(Escala, Destino, Dia, NuevoHoraActual, NuevoCostoT, TiempoMinimoConexion, RestoDestinos).

/*
    Dadas dos ciudades (origen y destino), determine la ruta de costo mínimo entre
    ellas, indicando las ciudades involucradas, hora de salida, hora de llegada y costo
*/
min_costo([], CostoMin, RutaMinCosto, CostoMin, RutaMinCosto).
min_costo([(Costo, Ruta)|RestoRutas], CostoActual, _, CostoMin, RutaMinCosto) :-
    Costo < CostoActual,
    min_costo(RestoRutas, Costo, Ruta, CostoMin, RutaMinCosto).
min_costo([(Costo, _)|RestoRutas], CostoActual, RutaActual, CostoMin, RutaMinCosto) :-
    Costo >= CostoActual,
    min_costo(RestoRutas, CostoActual, RutaActual, CostoMin, RutaMinCosto).

ruta_min_costo(Origen, Destino, Dia, TiempoMin, RutaMinCosto, CostoMin, HoraSalida, HoraLlegada) :-
    findall((Costo, Ruta), (rutas(Origen, Destino, Dia, 0, 0, TiempoMin, Ruta), obtener_costo_ruta(Ruta, Costo)), Rutas),
    Rutas = [(Costo, Ruta)|RestoRutas],
    min_costo(RestoRutas, Costo, Ruta, CostoMin, RutaMinCosto),
    obtener_hora_salida_ruta(RutaMinCosto, HoraSalida),
    obtener_hora_llegada_ruta(RutaMinCosto, HoraLlegada).

/*
    Mensaje de bienvenida
*/
main :-
    write('Bienvenido al sistema de vuelos\n'),
    write('OPCIONES:\n'),
    write('--- Cargar vuelos: leer_vuelos(<nombre_archivo>.csv)\n'),
    write('--- Ver vuelos desde una ciudad: vuelos_desde_ciudad(<ciudad>)\n'),
    write('--- Ver vuelos a una ciudad: vuelos_a_ciudad(<ciudad>)\n'),
    write('--- Ver rutas entre dos ciudades: rutas(<origen>, <destino>, <l,m,x,j,v,s,d>, 0, 0, <tiempo_min_conexion>, Rutas)\n'),
    write('--- Ver ruta de costo mínimo entre dos ciudades: ruta_min_costo(<origen>, <destino>, <l,m,x,j,v,s,d>, <tiempo_min_conexion>, Ruta, Costo, Salida, Llegada)\n'),
    write('\n').

:- main.
