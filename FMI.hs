--1

data Pais = UnPais { ingresoPerCapita::Int, poblacionActivaSPublico::Int, poblacionActivaSPrivado::Int, recursosNaturales::[String] , deudaFMI::Int } deriving Show

nimibia = UnPais 4140 400000 650000 ["minería", "ecoturismo"] 50000000
argentina = UnPais 9000 20000000 5000000 ["minería", "turismo", "ganadería", "petróleo"] 100000000

listaPaises = [nimibia, argentina]

--2

-- prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)

type RecetaFMI = Pais -> Pais

prestamoNMillones :: Int -> RecetaFMI
prestamoNMillones nMillones unPais = unPais { deudaFMI = (deudaFMI unPais) + (round.(*1.5).fromIntegral $ nMillones ) }

-- reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público y además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario

reduccionIPCSectorPublico :: Pais -> Int
reduccionIPCSectorPublico unPais    | poblacionActivaSPublico unPais > 100 = round.(* 0.8).fromIntegral.ingresoPerCapita $ unPais
                                    | otherwise = round.(* 0.85).fromIntegral.ingresoPerCapita $ unPais

reducirSectorPublico :: Int -> RecetaFMI
reducirSectorPublico cantidad unPais = unPais { poblacionActivaSPublico = (poblacionActivaSPublico unPais) - cantidad, ingresoPerCapita = reduccionIPCSectorPublico unPais }

-- darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares la deuda que el país mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso.
darRecursoNatural :: String -> RecetaFMI
darRecursoNatural unRecurso unPais = unPais { deudaFMI = (subtract 2000000).deudaFMI $ unPais, recursosNaturales = (filter (\r -> r /= unRecurso)).recursosNaturales $ unPais }

-- establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. Evitar la repetición de código.

calculoPBI :: Pais -> Int
calculoPBI unPais = (* ((poblacionActivaSPublico unPais) + (poblacionActivaSPrivado unPais)) ).ingresoPerCapita $ unPais 

blindaje :: RecetaFMI
blindaje unPais = (reducirSectorPublico 500).prestamoNMillones ((`div` 2).calculoPBI $ unPais) $ unPais

--3
-- (2 puntos)
-- a) Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país.

prestamoPorMineria :: RecetaFMI
prestamoPorMineria unPais = (darRecursoNatural "minería").(prestamoNMillones 200000000) $ unPais

-- b) Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral.
-- *Main> prestamoPorMineria nimibia
-- UnPais {ingresoPerCapita = 4140, poblacionActivaSPublico = 400000, poblacionActivaSPrivado = 650000, recursosNaturales = ["ecoturismo"], deudaFMI = 348000000}

-- la funcion "prestamoPorMineria" recibe un pais y luego le aplica la función de forma parcial "prestamoNMilones 200000000" que le otorga al país un prestamo por esa cantidad de dinero endeudandolo por 150% más de esos N millones, en este caso 200.000.000 * 1,5 = 350.000.000. Luego de esta función se le aplica de forma parcial "darRecursoNatural "minería"" que toma al país y le quita el recurso natural y se lo da a una empresa a fin al FMI y además le resta 2.000.000 de deuda con el FMI o sea que 350.000.000 - 2.000.000 = 348.000.000

--4

-- (3 puntos) Resolver todo el punto con orden superior, composición y aplicación parcial, no puede utilizar funciones auxiliares.
-- a) Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales.
paisesQueZafan :: [Pais] -> [Pais]
paisesQueZafan paises = filter (\p -> (elem "petróleo").recursosNaturales $ p) paises 

-- b) Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
totalDeudaFMI :: [Pais] -> Int
totalDeudaFMI paises = sum.map (\p -> deudaFMI p) $ paises

-- c) Indicar en dónde apareció cada uno de los conceptos (solo una vez) y justificar qué ventaja tuvo para resolver el requerimiento.

-- * orden superior : es una característica de funciones que reciben como argumento otra función la cual utilizan para obtener un resultado. En este caso en paisesQueZafan hemos utilizado la función de orden superior filter y en totalDeudaFMI usamos map. La ventaja de este tipo de funciones es que sirven para abstraer la lógica que hay en común en un algoritmo y dejar de lado los detalles particulares de una implementación. En el caso de filter podemos utilizar la misma funcion filter para diferentes casos sin necesidad de crear una nueva por cada criterio diferente de filtrado que pueda llegar a existir. Lo mismo sucede con map , no tenemos que crear una funcion para transformar un listado de paises a enteros o un listado de paises a floats, etc.

-- composicion: la utilizamos para encadenar invocaciones a funciones para obtener a partir de un tipo de dato dado un resultado que puede estar dentro del mismo tipo o fuera de él en otro conjunto. En el caso de paisesQueZafan compusimos filter y recursosNaturales. y en el caso de totalDeudaFMI compusimos sum y map.
-- La ventaja de la composición es que podemos abstraer un argumento común y dejarlo en un sólo lado de la ecuación y encadenar las imagenes de las funciones con los dominios de las siguientes en la cadena de llamado.

-- aplicación parcial: utilizamos aplicación parcial cuando no terminamos de aclarar la lista total de argumentos de una función y su invocación queda demorada hasta que recibe el útimo argumento que necesita. En paisesQueZafan aplicamos parcialmente la función elem "petróleo" que devuelve un booleano indicando si petróleo forma parte de la lista que recibe como argumento.

--5

-- (2 puntos) Debe resolver este punto con recursividad: dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor. Recordamos que el Producto Bruto Interno surge de multiplicar el ingreso per cápita por la población activa (privada y pública). 

recetasOrdenadas :: Pais -> [(RecetaFMI)] -> Bool
recetasOrdenadas unPais [] = False
recetasOrdenadas unPais [x] = (calculoPBI.x $ unPais) > (calculoPBI unPais)
recetasOrdenadas unPais (x:xs) = (calculoPBI.x $ unPais) > (calculoPBI unPais) && (recetasOrdenadas (x unPais) xs)

-- 6) (1 punto) Si un país tiene infinitos recursos naturales, modelado con esta función
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

paisConInfinitosRecursos = UnPais 1000 100000 200000 recursosNaturalesInfinitos 1000000 

-- a) ¿qué sucede evaluamos la función 4a con ese país? 

-- si evaluamos la función paisesQueZafan paisConInfinitosRecursos no podrá terminar ya que nunca podrá terminar de comprobar que no tiene petróleo entre sus recursos ya que la lista es recursiva y es infinita.

-- b) ¿y con la 4b?

-- si evaluamos la función totalDeudaFMI paises ++ [paisConInfinitosRecursos] va a poder terminar el programa ya que el listado de recursos no forma parte del calculo necesario para conocer el total de deuda del FMI.

-- *Main> totalDeudaFMI (listaPaises ++ [paisConInfinitosRecursos])
-- 151000000

-- Justifique ambos puntos relacionándolos con algún concepto.
