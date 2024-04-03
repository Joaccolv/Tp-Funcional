{-\Desde tiempos inmemoriales los viajes en el tiempo han seducido a muchas personas.
¡Hoy los vamos a programar!
Como cualquier viaje, tenemos a la persona que lo realiza y de ella se conoce:
    *su nombre,
    *su edad,
    *los recuerdos que obtuvo durante los viajes que hizo,
    *los viajes que realizó.
Al realizar un viaje (llegar al destino) la persona que lo hace puede sufrir distintos cambios o transformaciones. Por ejemplo, una transformación podría ser que si viaja al lejano oeste pierde todos los recuerdos que empiezan con una vocal. Otra podría ser que si viaja al futuro a Hill Valley su edad aumenta, porque se encuentra con su hijo. Las transformaciones son propias del viaje que realiza y podrían ser aún más que las dos que se nombran como ejemplo.
Además, de cada viaje conocemos:
Si van al futuro o al pasado
El nombre del lugar donde van
Una lista de transformaciones que le suceden al viajero al llegar a ese lugar
Si el viaje es al pasado entonces sabemos los recuerdos. Si el viaje es al futuro sabemos la cantidad de años luz que el viajero tuvo que realizar.
El año al que están viajando
De los recuerdos conocemos:
Su nombre del recuerdo
El lugar de donde proviene
Se pide:
1. Definir las funciones que permitan obtener:
    a. Dado un viajero su nombre,
    b. Dado un viaje, su nombre
    c. Dado un recuerdo, su nombre y el lugar de donde proviene
2. Definir una función que permita obtener los recuerdos y los viajes de un viajero.
3. Hacer una función que permita saber si un viaje es interesante. Un viaje es interesante si:
    a. Si el destino del viaje es el lejano oeste
    b. Si es un viaje al pasado y el viajero se puede traer más de 5 recuerdos
    c. Todos los viajes al futuro son interesantes.
4. Hacer una función que dada una lista de viajes, permita mostrar los nombres y los años de todos los viajes que son interesantes.
5. Hacer una función que dada una lista de viajes, un año inicio y un año fin, se pueda obtener cuáles son los nombres y el año de todos los viajes entre dos años que están en el rango pasado por parámetro.
6. Definir una función que permita hacer que el viajero realice una lista de viajes, se le apliquen las transformaciones necesarias y obtenga los recuerdos.
7. Hacer la función estadística que reciba una función de condición, una función de transformación y una lista. Luego, usarla para resolver las siguientes consultas:
    a. Dada una lista, encontrar todos los nombres de los viajes que tienen más de 3 transformaciones.
    b. Dada una lista de viajes, obtener la suma de todos los años luz que suman
    c. Dada una lista, obtener los nombres de todos los viajes. Tener en cuenta que los viajes al pasado no suman años luz.
Nota: sólo se puede hacer la función estadística y usar la misma en forma de consulta en los puntos a, b y c. No se pueden usar funciones-}

-- esto es una prueba 
-- probando ando

data Viajero = Viajero {nombre::String , edad::Int, recuerdos::[Recuerdos], viajes::[Viaje]} deriving Show

data Viaje = Viaje { tipoViaje:: TipoViaje, lugar::String, transformaciones::[String], recuerdo::[Recuerdos], 
                        aniosLuz::Int, anioAlQViajan::Int } deriving Show

data Recuerdos = Recuerdos {nombreDelRecuerdo ::  String, lugarOrigen:: String} deriving Show

data TipoViaje = Pasado | Futuro deriving (Eq, Show)


--transformaciones= perder recuerdos que comienzan con vocales si viajan a lejano oeste, aumentar 10 años si van hill valley


--viajeroEjemplo :: Viajero 
--viajeroEjemplo = Viajero "Estefania" 20 [Recuerdos "Recuerdo1" "Lugar1", Recuerdos "Recuerdo2" "Lugar2"] [viaje1, viaje2]
 -- where
    --viaje1 = Viaje Pasado "Travesia1" [] [] 0 0
   -- viaje2 = Viaje Pasado "Travesia2" [] [] 0 0


--1a
nombreViajero :: Viajero -> String
nombreViajero (Viajero nombre _ _ _) = nombre 
-- la llamo como nombreViajero viajeroEjemplo

--1b
nombreViaje :: Viaje -> String
nombreViaje = lugar

--1c
obtenerRecuerdo :: Recuerdos -> (String, String)
obtenerRecuerdo (Recuerdos nombreDelRecuerdo lugarOrigen) = (nombreDelRecuerdo, lugarOrigen)

--2
recuerdosYlugares :: Viajero -> ([Recuerdos], [Viaje])
recuerdosYlugares (Viajero _ _ recuerdos viajes) = (recuerdos, viajes) 

--3
esViajeInteresante :: Viaje -> Bool
esViajeInteresante (Viaje tipoViaje lugar transformaciones _ _ _ )
                | lugar == "Lejano Oeste" = True
                | tipoViaje == Pasado && length transformaciones > 5 = True
                | tipoViaje == Futuro = True
                | otherwise = False

--4
viajesInteresantes = filter esViajeInteresante
nombresYAñosViajesInteresantes :: [Viaje] -> [(String, Int)]
nombresYAñosViajesInteresantes viajes = [(nombreViaje viaje, anioAlQViajan viaje) | viaje <- viajes]

--5
viajesEntreAños :: [Viaje] -> Int -> Int -> [(String, Int)]
viajesEntreAños viajes añoInicio añoFin = [(nombreViaje viaje, anioAlQViajan viaje) | viaje <- viajes, añoInicio <= anioAlQViajan viaje, anioAlQViajan viaje <= añoFin]

--6
listaViajes :: [a1] -> [(a2, a1 -> String, c, a1 -> p, e, f)     -> [Recuerdos] -> [Recuerdos]]
listaViajes viajes = map transformacionesV viajes

obtenerNRecuerdo :: Viaje -> [Recuerdos]
obtenerNRecuerdo (Viaje _ _ _ recuerdo _ _) =recuerdo


transformacionesV :: t -> (a, t -> String, c, t -> p, e, f) -> [Recuerdos] -> [Recuerdos]
transformacionesV viaje (_, lugar, _, recuerdo, anioLuz, anioAlQViajan)  |lugar viaje == "lejano oeste" = filtrarRecuerdosVocal (recuerdo viaje)

filtrarRecuerdosVocal :: p -> [Recuerdos] -> [Recuerdos]
filtrarRecuerdosVocal recuerdo= filter (\recuerdo -> not (comienzaVocal (nombreDelRecuerdo recuerdo)))

comienzaVocal :: String -> Bool
comienzaVocal (x:_) = x `elem` "aeiou"

--7
-- Función estadística
funcionEstadistica :: (a -> Bool) -> ([b] -> c) -> [a] -> c
funcionEstadistica condicion transformacion elementos = transformacion [x | x <- elementos, condicion x]

--7a
viajesCMTresTransformaciones :: [Viaje] -> [String]
viajesCMTresTransformaciones = funcionEstadistica (\viaje -> length (transformaciones viaje) > 3) (map lugar)

--7b
sumaAniosLuz :: [Viaje] -> Int
sumaAniosLuz = funcionEstadistica (\_ -> True) (sum . map aniosLuz)

--7c
nombresDeViajes :: [Viaje] -> [String]
nombresDeViajes = funcionEstadistica (\_ -> True) (map lugar)
