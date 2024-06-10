{-Desde tiempos inmemoriales los viajes en el tiempo han seducido a muchas personas.
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

data Viajero = Viajero String Int [Recuerdo] [Viaje]  
data Viaje = Pasado String [Viajero -> Viajero] [Recuerdo] Int | Futuro String [Viajero -> Viajero] Int Int 
data Recuerdo = Recuerdo String String deriving (Show,Eq)

instance Show Viaje where 
    show (Pasado nombre lugares transformaciones anio) = "Viaje pasado {nombre = " ++ nombre ++ ", lugares = " ++ "Hola" ++ ", transformaciones = <funciones> , anio = " ++ show anio ++"}" 
    show (Futuro nombre lugares aniosluz anio) = "Viaje futuro {nombre = " ++ nombre ++ ", lugares = " ++ "Hola" ++ ", aniosluz = " ++ show aniosluz ++ ", anio = " ++ show anio ++ "}"

instance Show Viajero where show (Viajero nombre edad recuerdos viajes) = "Persona {nombre = " ++ nombre ++ ", edad = " ++ show edad ++ ", recuerdos = " ++ show recuerdos ++ ", viajes = " ++ show viajes ++ "}"

--1c
nombreViajero :: Viajero -> String
nombreViajero(Viajero nombre _ _ _) = nombre

nombreLugarV :: Viaje -> String
nombreLugarV(Pasado nombre _ _ _) = nombre
nombreLugarV(Futuro nombre _ _ _) = nombre
obtenerRecuerdo (Recuerdo nombreDelRecuerdo lugarOrigen) = (nombreDelRecuerdo, lugarOrigen)

--2

recuerdosYLugares :: Viajero -> ([Recuerdo], [Viaje])
recuerdosYLugares (Viajero _ _ recuerdos viajes) = (recuerdos, viajes) 

--3

esViajeInteresante :: Viaje -> Bool
esViajeInteresante (Pasado "Lejano Oeste" _ _ _) = True 
esViajeInteresante (Pasado _ _ listaRecuerdos _) = length listaRecuerdos > 5                                                                                  
esViajeInteresante (Futuro _ _ _ _) = True
esViajeInteresante _ = False

--4

nYAQueViaje :: Viaje -> (String, Int)
nYAQueViaje (Pasado lugarviaje _ _ anio) = (lugarviaje,anio)
nYAQueViaje (Futuro lugarviaje _ _ anio) = (lugarviaje,anio)
nYAQueViajanDeViajesINteresantes :: [Viaje] -> [(String, Int)]
nYAQueViajanDeViajesINteresantes viajes = map nYAQueViaje (filter esViajeInteresante viajes)
--5
viajesEntreAnios :: [Viaje] -> Int -> Int -> [(String, Int)]
viajesEntreAnios listaViajes anioInicio anioFin = map nYAQueViaje (filter (\viaje -> snd (nYAQueViaje viaje) >= anioInicio && snd (nYAQueViaje viaje) <= anioFin) listaViajes)
--6
viajar :: Foldable t => Viajero -> t Viaje -> Viajero
viajar viajero viajes = foldl (\ acc x -> hacerViaje acc x)  viajero  viajes

hacerViaje :: Viajero -> Viaje -> Viajero
hacerViaje (Viajero nombre edad recuerdosViajero viajesActuales) viaje = foldl (\ acc x -> x acc)  (Viajero nombre edad  ( recuerdosViajero ++ recuerdos viaje ) (viajesActuales ++ [viaje])) (transformaciones viaje)

transformaciones :: Viaje -> [Viajero -> Viajero]
transformaciones (Pasado _  listaTransformaciones _ _ ) = listaTransformaciones
transformaciones (Futuro _ listaTransformaciones _ _) = listaTransformaciones

recuerdos :: Viaje -> [Recuerdo]
recuerdos  (Pasado _ _ listaRecuerdos _ ) = listaRecuerdos 
recuerdos (Futuro _ _ _ _ ) = [] 


--ejemplos transformaciones

aumentarEdad :: Viajero -> Viajero
aumentarEdad (Viajero nombre edad recuerdos viajes) = Viajero nombre (edad + 1) recuerdos viajes

disminuirEdad :: Viajero -> Viajero
disminuirEdad (Viajero nombre edad recuerdos viajes) = Viajero nombre (edad - 1) recuerdos viajes

-- 7 Función estadística


estadistica :: (a -> Bool) -> (a -> b) -> [a] -> [b]
estadistica condicion transformacion lista = map transformacion (filter condicion lista)
{- Dejo las consultas aca por las dudas, igual van a estar en el txt junto a la lista de viajes
Consultas:
a- estadistica ((>3).length).transformaciones nombreLugarV [viajePasado,viajeFuturo,viajeAChina]

b- sum (estadistica esViajeFuturo (\(Futuro _ _ _ aniosLuz) -> aniosLuz) [viajePasado, viajeFuturo, viajeAChina, viajePrueba])

c- estadistica (\viaje -> True) nombreLugarV [viajePasado, viajeFuturo, viajeAChina, viajeDeAmigos]
  
-}

esViajeFuturo :: Viaje -> Bool
esViajeFuturo (Pasado _ _ _ _) = False
esViajeFuturo (Futuro _ _ _ _) = True

viajero1 :: Viajero
viajero1 = Viajero "Pedro" 35 [] []
viajero2 :: Viajero
viajero2 = Viajero "Carlos" 19 [] []
juan :: Viajero
juan = Viajero "Juan" 23 [] []
marty :: Viajero
marty = Viajero "Marty McFly" 17 [recuerdoBaile,recuerdoPersecucion,recuerdoTiburon] [viajePasado,viajeFuturo,viajeAChina]
doc :: Viajero
doc = Viajero "Dr Emmet Brown" 65 [] [viajePasado,viajeFuturo,viajeAChina]

recuerdoBaile :: Recuerdo
recuerdoBaile = Recuerdo "Baile del encanto bajo el oceano" "Escuela Secundaria"
recuerdoPersecucion :: Recuerdo
recuerdoPersecucion = Recuerdo "Persecucion con Biff" "Autopista"
recuerdoTiburon :: Recuerdo
recuerdoTiburon = Recuerdo "Holograma de Jaws 19" "Plaza"
recuerdoPartido :: Recuerdo
recuerdoPartido = Recuerdo "Partido entre amigos" "Cancha de independiente"
recuerdoChoque :: Recuerdo
recuerdoChoque = Recuerdo "Choque Automovilistico" "Ruta 11"
recuerdoHamburguesa :: Recuerdo
recuerdoHamburguesa = Recuerdo "Hamburguesa triple" "Burger King"
recuerdoVaqueros :: Recuerdo
recuerdoVaqueros = Recuerdo "Sombrero" "Texas"

viajePasado :: Viaje
viajePasado = Pasado "Marte" [] [] 1950
viajeFuturo :: Viaje
viajeFuturo = Futuro "Luna" [] 300 2010
viajeAChina :: Viaje
viajeAChina = Futuro "China" [] 100 1985
viajePrueba :: Viaje
viajePrueba = Pasado "Lejano Oeste" [aumentarEdad] [] 2005
viajeDeAmigos :: Viaje
viajeDeAmigos = Pasado "Tokyo" [aumentarEdad] [recuerdoBaile,recuerdoPersecucion,recuerdoTiburon, recuerdoChoque, recuerdoHamburguesa, recuerdoPartido] 1930
viajeTexas :: Viaje
viajeTexas = Pasado "Lejano Oeste" [aumentarEdad] [recuerdoVaqueros] 2018
viajeTurista :: Viaje
viajeTurista = Futuro "Caminito" [disminuirEdad] 346 2040