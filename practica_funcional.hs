siguiente :: Integer -> Integer
siguiente numero = numero + 1

doble :: Integer -> Integer
doble numero = numero * 2

calcular :: (Integer, Integer) -> (Integer, Integer)
calcular (x, y) | even x && even y = (doble x, y)
                | even x && odd y = (doble x, siguiente y)
                | odd x && odd y = (x, siguiente y)
                | odd x && even y = (x, y)

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

type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)
notaMaxima :: Alumno -> Nota
notaMaxima (_, x, y, z) = (max x.max y) z

cuadruple numero = (doble.doble) numero

esMayorA :: Integer -> Bool
esMayorA numero = (doble.siguiente.(+) 2) numero > 10

lambdaTriple numero = (\x -> x * 3) numero

lambdaSiguiente numero =  (\x -> x + 1) numero

lambdaSumarDos n1 n2 = (\x y -> x + y) n1 n2