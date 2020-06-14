data Ciudadano = UnCiudadano { profesion::String, sueldoAnual::Int, cantidadHijos::Int, bienes::[(String,Int)] } deriving Show

homero = UnCiudadano "SeguridadNuclear" 9000 3 [("casa",50000), ("deuda",-70000)]
frink = UnCiudadano "Profesor" 12000 1 []
krabappel = UnCiudadano "Profesor" 12000 0 [("casa",35000)]
burns = UnCiudadano "Empresario" 300000 1 [("empresa",1000000),("empresa",500000),("auto",200000)]

springfield = [homero, burns, frink, krabappel]

-- Se piden las siguientes funciones. Sólo puede usarse recursividad en
-- los puntos 4 ó 5, aunque no es necesaria.

-- 1. diferenciaDePatrimonio: recibe una ciudad y dice cuál es la
-- diferencia entre el ciudadano que más patrimonio tiene y el que
-- menos patrimonio tiene. El patrimonio de cada ciudadano se
-- obtiene sumando el valor de su sueldo y de sus bienes.
type Ciudad = [Ciudadano]

calculoPatrimonio :: Ciudadano -> Int
calculoPatrimonio ciudadano = sueldoAnual ciudadano + (sum.(map snd).bienes) ciudadano

maximoCiudadano :: Ciudadano -> Ciudadano -> Ciudadano
maximoCiudadano c1 c2 | calculoPatrimonio c1 >= calculoPatrimonio c2 = c1
                      | otherwise = c2

minimoCiudadano :: Ciudadano -> Ciudadano -> Ciudadano
minimoCiudadano c1 c2 | calculoPatrimonio c1 <= calculoPatrimonio c2 = c1
                      | otherwise = c2

diferenciaDePatrimonio :: Ciudad -> Int
diferenciaDePatrimonio ciudad = (calculoPatrimonio.(foldl1 maximoCiudadano)) ciudad - (calculoPatrimonio.(foldl1 minimoCiudadano)) ciudad

-- 2. tieneAutoAltaGama: recibe un ciudadano y dice si tiene un
-- auto de alta gama, ó sea, si tiene entre sus bienes un auto que valga más de 100000.

tieneAutoAltaGama :: Ciudadano -> Bool
tieneAutoAltaGama ciudadano = (any esAutoAltaGama .bienes) ciudadano

esAutoAltaGama :: (String, Int) -> Bool
esAutoAltaGama ("auto", valor) = valor > 100000
esAutoAltaGama _ = False

-- 3. Medidas: se aplican a un ciudadano, y lo retornan modificado.
type MedidaGobierno = Ciudadano -> Ciudadano

-- a. auh: Hace que aumente el sueldo de la persona en 1000 por cada hijo, si el patrimonio de la
-- persona es menor a 0 (en otro caso, el ciudadano no cambia).
-- Main> auh (UnCiudadano “Doctor” 2000 2 [(“deuda”,-3000)])
-- UnCiudadano “Doctor” 4000 2 [(“deuda”,-3000)]

auh :: MedidaGobierno
auh ciudadano   | calculoPatrimonio ciudadano < 0 = ciudadano { sueldoAnual = ((+ sueldoAnual ciudadano).(* 1000).cantidadHijos) ciudadano }
                | otherwise = ciudadano

-- b. impuestoGanancias: si el sueldo supera el mínimo dado , disminuye su sueldo el 30% de la
-- diferencia. Si no supera el mínimo, queda igual.
-- Main> impuestoGanancias 20000 (UnCiudadano “Doctor” 20100 0 [])
-- UnCiudadano “Doctor” 20070 0 []

impuestoGanancias :: Int -> MedidaGobierno
impuestoGanancias minimo ciudadano  | sueldoAnual ciudadano > minimo = ciudadano { sueldoAnual = sueldoAnual ciudadano - (round.(* 0.3).fromIntegral) (sueldoAnual ciudadano - minimo) }
                                    | otherwise = ciudadano

-- c. impuestoAltaGama: si el ciudadano tiene algún auto de alta gama, disminuye su sueldo en un
-- 10% del valor del auto, sino no disminuye nada.
-- Main> impuestoAltaGama (UnCiudadano “Profesor” 30000 0 [(“auto”,120000)])
-- UnCiudadano “Profesor” 18000 0 [(“auto”,120000)]

impuestoAltaGama :: MedidaGobierno
impuestoAltaGama ciudadano = ciudadano { sueldoAnual = sueldoAnual ciudadano - (round.(* 0.1).fromIntegral.valorAutoAltaGama) ciudadano }

autoAltaGama :: [(String, Int)] -> [(String, Int)]
autoAltaGama bienes = filter esAutoAltaGama bienes 

valorAutoAltaGama:: Ciudadano -> Int
valorAutoAltaGama ciudadano | (length.autoAltaGama.bienes) ciudadano > 0 = (snd.head.autoAltaGama.bienes) ciudadano
                            | otherwise = 0

-- d. negociarSueldoProfesion: Esta medida recibe una profesión y un porcentaje. Si el ciudadano
-- tiene esa profesión, entonces aumenta su sueldo el porcentaje indicado. Si no es su profesión,
-- entonces queda igual.

type NombreProfesion = String

negociarSueldoProfesion :: NombreProfesion -> Float -> MedidaGobierno
negociarSueldoProfesion nombreProf porcentaje ciudadano | profesion ciudadano == nombreProf = ciudadano { sueldoAnual = (round.(* (1.0 + porcentaje / 100.0)).fromIntegral.sueldoAnual) ciudadano }
                                                        | otherwise = ciudadano

-- Sólo en los puntos siguientes puede usarse recursividad, aunque no es necesaria:

-- 4. Un gobierno está representado por la lista de años que gobernó, y por la lista de medidas que aplicó.
data Gobierno = UnGobierno { años::[Int], medidas::[ (MedidaGobierno) ] }

-- a. Escribir la función constante gobiernoA, que devuelve un gobierno que gobernó entre 1999 y
-- 2003, y aplicó impuesto a las ganancias de 30.000, negoció sueldo de profesores por 10%,
-- negoció sueldo de empresarios por 40%, y aplicó impuesto a vehículos de alta gama y también
-- aplicó la auh.

gobiernoA = UnGobierno [1999..2003] [impuestoGanancias 30000, negociarSueldoProfesion "Profesor" 10, negociarSueldoProfesion "Empresario" 40, impuestoAltaGama, auh]

-- b. Escribir la función constante gobiernoB, que devuelve un gobierno que gobernó desde 2004
-- hasta 2008, y aplicó impuesto a las ganancias de 40.000, negoció sueldo de profesores por
-- 30%, y negoció sueldo de camioneros por 40%.

gobiernoB = UnGobierno [2004..2008] [impuestoGanancias 40000, negociarSueldoProfesion "Profesor" 30, negociarSueldoProfesion "Camionero" 40]

gobiernoC = UnGobierno [2015..2019] [impuestoGanancias 1000000, negociarSueldoProfesion "Empresario" 90]

-- c. Hacer la función gobernarUnAño, que recibe un gobierno y una ciudad, y aplica todas las
-- medidas del gobierno a cada uno de los ciudadanos de la ciudad. Retorna la ciudad cambiada.
aplicarMedidas :: [(MedidaGobierno)] -> MedidaGobierno
aplicarMedidas medidas unCiudadano = foldl (\c m -> m c) unCiudadano medidas

gobernarUnAño :: Gobierno -> Ciudad -> Ciudad
gobernarUnAño gobierno ciudad = map (\c -> aplicarMedidas (medidas gobierno) c ) ciudad

-- d. Hacer la función gobernarPeriodoCompleto, que recibe un gobierno y una ciudad, y gobierna
-- a esa ciudad todos los años del período (Es decir, gobierna tantos años a la ciudad como haya
-- durado el mandato).

gobernarPeriodoCompleto :: Gobierno -> Ciudad -> Ciudad
gobernarPeriodoCompleto gobierno ciudad = foldl (flip gobernarUnAño) ciudad (replicate (length.años $ gobierno) gobierno)

-- e. Hacer la función distribuyóRiqueza, que dice si un gobierno hizo justamente eso en una
-- ciudad. Esto es cuando luego de gobernar por todo su período, la diferencia de patrimonio es
-- menor que al iniciar.

distribuyoRiqueza :: Gobierno -> Ciudad -> Bool
distribuyoRiqueza unGobierno unaCiudad = (> diferenciaDePatrimonio (gobernarPeriodoCompleto unGobierno unaCiudad)).diferenciaDePatrimonio $ unaCiudad

-- 5. Un ciudadano especial

-- a. Crear al ciudadano kane, que no tiene hijos, es empresario, tiene 100000 de sueldo, e infinitos
-- trineos, todos llamados “Rosebud”, y cada uno $5 más caro que el anterior.
-- Main> kane
-- UnCiudadano “Empresario” 100000 0
-- [(“Rosebud”,5),(“Rosebud”,10),(“Rosebud”,15)..... y así...

infinitosTrineos = zip (repeat "Rosebud") (map (*5) [1..])

kane = UnCiudadano "Empresario" 1000000 0 infinitosTrineos

-- b. Decir qué sucede con lo siguiente. Justificar conceptualmente ambas respuestas.
-- i. Main> gobernarUnAño gobiernoA [kane]

-- En esta invocación como gobiernoA tiene una medida específica que es impuestoAltaGama y en ella se consideran los bienes de un ciudadano.
-- Como ciudadano Kane tiene infinitos bienes esta función nunca podrá terminar de procesar los bienes para verificar si tiene un auto de alta gama.

-- ii. Main> gobernarUnAño gobiernoB [kane]

-- En esta invocación gobiernoB no posee ninguna medida que necesite usar los bienes de Kane para realizar algún calculo.
-- Pero como el constructor de un ciudadano posee la sentencia "deriving Show" que permite imprimir los valores de las propiedades internas de este tipo de dato, entonces
-- al momento de querer devolver el resultado nunca podrá terminar de recorrer la lista de bienes, ya que es infinita.

-- Observación: Si removemos deriving Show de UnCiudadano, podremos realizar la invocación de "gobernarUnAño gobiernoB [kane]" sin problemas.
-- Sólo que para ver los datos tendremos que acceder individualmente a cada propiedad del tipo de dato.

-- 6. Dar el tipo más genérico de f1:
-- f1 x y z = map (*x) . filter (y z)

-- x :: a , tal que a es un tipo Num que admite ser multiplicado con *
-- y :: (b -> a -> Bool) , es una función que toma como argumento un tipo desconocido y un tipo a (Num) ya que debe cumplir las condiciones de filter. Debe proporcionar una función que dado un único tipo a coincidente con el tipo de la lista devuelve True o False si pasa el criterio de filtro o no.
-- z :: b , es el otro tipo genérico que no conocemos (podría ser cualquier tipo) dependiendo de las operaciones que haga "y" con "b" y "a" para decidir si a pasa o no el filtro, dichas operaciones no están especificadas.
-- valor de retorno :: ([a] -> [a]) es otra función ya que la operación más externa de todas es una composición de funciones entre un map y un filter. Con lo cual es una función que como argumento toma una lista de "a": [a] y como la opera con una operación LCI devuelve el mismo tipo [a].

f1 :: (Num a) => a -> (b -> a -> Bool) -> b -> ([a] -> [a])
f1 x y z = map (*x) . filter (y z)