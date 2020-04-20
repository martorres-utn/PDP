siguiente :: Integer -> Integer
siguiente numero = numero + 1

doble :: Integer -> Integer
doble numero = numero * 2

-- 1) Definir la función calcular, que recibe una tupla de 2 elementos, y devuelve una nueva tupla según las siguientes reglas:
-- *  si el primer elemento es par lo duplica; si no lo deja como está
-- *  si el segundo elemento es impar le suma 1; si no deja como está
-- >calcular (4,5)
-- (8,6)
calcular :: (Integer, Integer) -> (Integer, Integer)
calcular (x, y) | even x && even y = (doble x, y)
                | even x && odd y = (doble x, siguiente y)
                | odd x && odd y = (x, siguiente y)
                | odd x && even y = (x, y)

-- 2) Definir las funciones boolenas estándar. Sin usar las funciones predefinidas.
-- 2.1) Definir la función and’
-- 2.2) Definir la función or’.
and' :: Bool -> Bool -> Bool
and' p1 p2  | p1 = p2
            | otherwise = False

and'' :: Bool -> Bool -> Bool
and'' True True = True
and'' _ _ = False

or' :: Bool -> Bool -> Bool
or' p1 p2 | p1 = True
          | p2 = True
          | otherwise = False

or'' :: Bool -> Bool -> Bool
or'' False False = False
or'' _ _ = True

-- 3) Definir la función notaMaxima que dado un alumno devuelva la máxima nota del alumno. (Nota resolverlo sin guardas).
-- type Nota = Integer
-- type Alumno = (String, Nota, Nota, Nota)
type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)
notaMaxima :: Alumno -> Nota
notaMaxima (_, x, y, z) = (max x.max y) z

-- 4) Definir la función cuadruple reutilizando la función doble. (usar composición)
cuadruple numero = (doble.doble) numero

-- 5) Definir la función esMayorA, que verifique si el doble del siguiente de la suma entre 2 y un número es mayor a 10. (usar composición)
esMayorA :: Integer -> Bool
esMayorA numero = (doble.siguiente.(+) 2) numero > 10

-- 6) Dar expresiones lambda que sean equivalentes a las siguientes expresiones:
-- triple
-- siguiente
-- suma
-- sumarDos
lambdaTriple numero = (\x -> x * 3) numero
lambdaSiguiente numero =  (\x -> x + 1) numero
lambdaSumarDos n1 n2 = (\x y -> x + y) n1 n2

-- 7)  Dada las siguientes definiciones:
-- 7.1) apply f x = f x
-- ¿ A qué se reduce la siguiente expresión ?.
-- > apply fst  (const 5 7, 4)

-- Aplica la funcion f al parametro x
apply f x = f x
-- apply fst (const 5 7, 4)
-- devuelve 5

-- 7.2) twice f x = (f . f) x
-- ¿ A qué se reduce la siguiente expresión ?.
-- >twice (`div` 2) 12

-- Aplica la composicion de f consigo misma al parametro x
twice f x = (f.f) x
-- twice ('div' 2) 12
-- devuelve 3