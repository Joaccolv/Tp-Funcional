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

                                                        --aca es el data que necesito moficiar
data Viaje = Viaje { tipoViaje:: TipoViaje, nombreLugar::String, transformaciones::[Viajero -> Viajero], recuerdo::[Recuerdos], 
                        aniosLuz::Int, anioAlQViajan::Int }

data Recuerdos = Recuerdos {nombreDelRecuerdo ::  String, lugarOrigen:: String} deriving Show

data TipoViaje = Pasado | Futuro deriving (Eq, Show)

instance Show Viaje where
    show (Viaje tipoViaje lugar _ recuerdos aniosLuz anioAlQViajan) = 
        "Viaje { tipoViaje = " ++ show tipoViaje ++
        ", lugar = " ++ show lugar ++
        ", transformaciones = <funciones>" ++
        ", recuerdos = " ++ show recuerdos ++
        ", aniosLuz = " ++ show aniosLuz ++
        ", anioAlQViajan = " ++ show anioAlQViajan ++
        " }"

viajeroEjemplo :: Viajero 
viajeroEjemplo = Viajero "Estefania" 20 [Recuerdos "Recuerdo1" "Lugar1", Recuerdos "Recuerdo2" "Lugar2"] [viaje1, viaje2]
  where
    viaje1 = Viaje Pasado "Travesia1" [] [] 0 0
    viaje2 = Viaje Pasado "Travesia2" [] [] 0 0

viajesEj :: [Viaje]
viajesEj = [viaje3, viaje4]

viaje3 :: Viaje
viaje4 :: Viaje
viaje3 = Viaje Futuro "Brasil" [] [] 0 0
viaje4 = Viaje Futuro "Costa" [] [] 0 0

--1c
obtenerRecuerdo :: Recuerdos -> (String, String)
obtenerRecuerdo (Recuerdos nombreDelRecuerdo lugarOrigen) = (nombreDelRecuerdo, lugarOrigen)

--2
recuerdosYlugares :: Viajero -> ([Recuerdos], [Viaje])
recuerdosYlugares (Viajero _ _ recuerdos viajes) = (recuerdos, viajes) 

--3
esViajeInteresante :: Viaje -> Bool
esViajeInteresante (Viaje Pasado _ recuerdo _ _ _ ) = length recuerdo > 5
esViajeInteresante (Viaje _ "Lejano Oeste" _ _ _ _) = True
esViajeInteresante (Viaje Futuro _ _ _ _ _) = True
esViajeInteresante _ = False

--4
nYAViajesInteresantes :: [Viaje] -> [(String, Int)]
nYAViajesInteresantes viajes = map (\viaje -> (nombreLugar viaje, anioAlQViajan viaje)) viajesInteresantes
    where viajesInteresantes = filter esViajeInteresante viajes
--5

viajesEntreAnios :: [Viaje] -> Int -> Int -> [(String, Int)]
viajesEntreAnios viajes anioInicio anioFin = nYAViajesInteresantes (filter (\viaje -> anioAlQViajan viaje >= anioInicio && anioAlQViajan viaje <= anioFin) viajes)

--6


viajar :: Foldable t => Viajero -> t Viaje -> Viajero
viajar viajero viajes = foldl (\ acc x -> hacerViaje acc x)  viajero  viajes


hacerViaje :: Viajero -> Viaje -> Viajero
hacerViaje (Viajero nombre edad recuerdosViajero viajesActuales) viaje = foldl (\ acc x -> x acc)  (Viajero nombre edad  (recuerdosViajero ++ recuerdosLista viaje ) (viajesActuales ++ [viaje])) (transformacionesLista viaje)

transformacionesLista :: Viaje -> [Viajero -> Viajero]
transformacionesLista (Viaje tipoViaje _ listaTransformaciones _ _ _) = listaTransformaciones

recuerdosLista :: Viaje -> [Recuerdos]
recuerdosLista (Viaje Pasado _ _ recuerdos _ _) = recuerdos 
recuerdosLista (Viaje Futuro _ _ _ _ _) = []

--ejemplos transformaciones
aumentarEdad :: Viajero -> Viajero
aumentarEdad (Viajero nombre edad recuerdos viajes) = Viajero nombre (edad + 1) recuerdos viajes
disminuirEdad :: Viajero -> Viajero
disminuirEdad (Viajero nombre edad recuerdos viajes) = Viajero nombre (edad - 1) recuerdos viajes

-- 7 Función estadística

estadistica :: (a -> Bool) -> (a -> b) -> [a] -> [b]
estadistica condicion transformacion elementos = map transformacion (filter condicion elementos)

{- Dejo las consultas aca por las dudas, igual van a estar en el txt junto a la lista de viajes
Consultas:
a- estadistica ((>3).length).transformaciones nombreLugarViaje [viajePasado,viajeFuturo,viajeAChina]

b- estadistica (\_ -> True) (sum . map aniosLuz) listaDviajes

c- estadistica (\viaje -> True) nombreLugarViaje [viajePasado, viajeFuturo, viajeAChina, viajeDeAmigos]   
-}



recuerdoBaile = Recuerdos "Baile del encanto bajo el oceano" "Escuela Secundaria"
recuerdoPersecucion = Recuerdos "Persecucion con Biff" "Autopista"
recuerdoTiburon = Recuerdos "Holograma de Jaws 19" "Plaza"
recuerdoPartido = Recuerdos "Partido entre amigos" "Cancha de independiente"
recuerdoChoque = Recuerdos "Choque Automovilistico" "Ruta 11"
recuerdoHamburguesa = Recuerdos "Hamburguesa triple" "Burger King"
recuerdoVaqueros = Recuerdos "Sombrero" "Texas"
viajePrueba = Viaje Pasado "Lejano Oeste" [aumentarEdad] [] 2005
viajeDeAmigos = Viaje Pasado "Tokyo" [aumentarEdad] [recuerdoBaile,recuerdoPersecucion,recuerdoTiburon, recuerdoChoque, recuerdoHamburguesa, recuerdoPartido] 1930
viajeTexas = Viaje Pasado "Lejano Oeste" [aumentarEdad] [recuerdoVaqueros] 2018
