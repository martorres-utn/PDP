-- 1) Dada una lista de tuplas, sacar la cantidad de elementos utilizando foldl y foldr.
-- Main>cantidadElementos [(8,6),(5,5),(5,6),(7,8)]
-- 4

contar sem _ = sem + 1

cantidadElementos :: [(Int, Int)] -> Int
cantidadElementos lista = foldl contar 0 lista

cantidadElementos' lista = foldl (\sem _ -> sem + 1) 0 lista

cantidadElementos'' lista = foldr (\_ sem -> sem + 1) 0 lista

-- 2) Dada una lista de pares (empleado, gasto), conocer el empleado más gastador usando foldl y foldr.
-- Main>masGastador [(“ana”,80),(“pepe”,40),(“juan”,300),(“maria”,120)]
-- (“juan”,300)
 
masGastador (x:xs) = foldl masGasta x xs

masGasta sem el   | snd sem > snd el = sem
                    | otherwise = el

masGastador' (x:xs) = foldr masGasta x xs

masGastadorFoldr (cab:cola) = foldr (flip masGasta) cab cola

-- 3) Dada una lista de (empleado, gasto), conocer el gasto total usando foldl y foldr.
-- Main>monto [(“ana”,80),(“pepe”,40),(“juan”,300),(“maria”,120)]
-- 540

--en estos casos resulta especialmente significativo el orden de la semilla y el argumento1 en la función fold

gastoTotalFoldL listaEmpleado = foldl (\sem x -> snd x + sem) 0 listaEmpleado

gastoTotalFoldR listaEmpleado = foldr (\x sem -> snd x + sem) 0 listaEmpleado

-- 4) Completar con lo que corresponda para:
-- >foldl …. 2 [(3+), (*2), (5+)]
-- 15

-- foldl (\sem f -> f sem) 2 [(3+), (*2), (5+)]

-- >foldr …. 2 [(3+), (*2), (5+)]
-- 17

-- foldr (\f sem -> f sem) 2 [(3+), (*2), (5+)]

-- foldl (flip ($)) 2 [(+5)]

-- 5) Dada una lista de proyectos

type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data  Proyecto = Proy { nombreProyecto::Nombre,  inversionInicial::InversionInicial, profesionales::Profesionales } deriving Show
proyectos = [(Proy "red social de arte"  20000 ["ing. en sistemas", "contador"]), (Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"]), (Proy "ventaChurro" 1000 ["cocinero"]) ]

-- 5.1) Determine una función que permita conocer el máximo proyecto según. Revolverlo usando foldl y foldr.
-- a) La inversión inicial
-- b) El nro de profesionales.
-- c) La cantidad de palabras del proyecto. 
-- Muestre por cada caso ejemplos de invocación y respuesta.

proyectoMaximo f a b | (f a) > (f b) = a
                    | (f a) < (f b) = b
                    | otherwise = a

maximoSegun f (x:xs) = foldl (proyectoMaximo f) x xs
maximoSegun' f (x:xs) = foldr (proyectoMaximo f) x xs

-- a) maximoSegun (inversionInicial) proyectos
-- b) maximoSegun (length.profesionales) proyectos
-- c) maximoSegun (length.words.nombreProyecto) proyectos