-- Un laboratorio está experimentando con distintos ratones para

-- crear nuevos medicamentos naturales.

-- De los ratones que usan se conoce nombre, edad, su peso, y las
-- enfermedades que posee.
data Animal = Raton {nombre :: String, edad :: Float, peso :: Float, enfermedades :: [String]} deriving Show

-- Ejemplo de raton
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]

-- 1. Hacer 4 funciones de modificación del ratón: modificarNombre, modificarEdad,
-- modificarPeso, modificarEnfermedades. Deben recibir una función y un ratón, y devolver el
-- ratón modificado.
-- Ejemplos:
-- > modificarEdad (*2) cerebro
-- Raton "Cerebro" 18.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
-- > modificarNombre (++ " el genio") cerebro
-- Raton "Cerebro el genio" 9.0 0.2 ["brucelosis", "sarampión",
-- "tuberculosis"]

modificarNombre :: (String -> String) -> Animal -> Animal
modificarNombre func raton = raton { nombre = (func.nombre) raton }

modificarEdad :: (Float -> Float) -> Animal -> Animal
modificarEdad func raton = raton { edad = (func.edad) raton }

modificarPeso :: (Float -> Float) -> Animal -> Animal
modificarPeso func raton = raton { peso = (func.peso) raton }

modificarEnfermedades :: ([String] -> [String]) -> Animal -> Animal
modificarEnfermedades func raton = raton { enfermedades = (func.enfermedades) raton }

-- Existen distintos tipos de hierbas que afectan de diferentes maneras al ratón. Definir dichas
-- hierbas:
-- a. hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.
-- Por ejemplo, si a cerebro le doy hierbaBuena, se transforma en un ratón de 3 años.
-- b. hierbaVerde, elimina una enfermedad dada.
-- Por ejemplo, si a cerebro le doy la hierbaVerde para la “brucelosis”, queda con
-- sarampión y con tuberculosis.
-- c. alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde
-- un 5%.
-- Por ejemplo, un ratón de 3 kg queda con 2,7 kg y cerebro queda con 0.19 kg.
-- d. hierbaMagica, hace que el ratón pierda todas sus infecciones y quede con 0 años de
-- edad.

hierbaBuena :: Animal -> Animal
hierbaBuena raton = (modificarEdad sqrt) raton

hierbaVerde :: String -> Animal -> Animal
hierbaVerde enf raton = (modificarEnfermedades (filter (\x -> x /= enf))) raton

alcachofa :: Animal -> Animal
alcachofa raton | peso raton > 2.0 = modificarPeso (*0.9) raton
                | otherwise = modificarPeso (*0.95) raton

hierbaMagica :: Animal -> Animal
hierbaMagica raton = ((modificarEnfermedades (filter (\x -> x /= x))).(modificarEdad (*0))) raton

-- 3. Medicamentos:

-- a. Hacer la función medicamento, que recibe una lista de hierbas, un ratón, y administra
-- al ratón todas las hierbas.

medicamento :: [(Animal -> Animal)] -> Animal -> Animal
medicamento [x] raton = x raton
medicamento (x:xs) raton = ((medicamento xs).x) raton

medicamento' hierbas raton = foldl (\r h-> h r) raton hierbas

-- b. Hacer antiAge que es un medicamento que está hecho con 3 hierbas buenas y una
-- alcachofa.

-- > antiAge (Raton "bicenterata" 256.0 0.2 [])
-- Raton "bicenterata" 2.0 0.19 []

repetir :: a -> [a]
repetir n = n : repetir n

antiAge :: Animal -> Animal
antiAge raton = medicamento (alcachofa:take 3 (repetir hierbaBuena)) raton

-- SE PUEDE USAR REPLICATE EN LUGAR DE REPETIR
antiAge' raton = medicamento (replicate 3 hierbaBuena ++ [alcachofa]) raton

-- c. Hacer reduceFatFast (que viene en distintas potencias) y es un medicamento
-- compuesto por una hierbaVerde de “obesidad” y tantas alcachofas como indique su
-- potencia. 
-- > reduceFatFast 1 (Raton “Orejudo" 4.0 10.0 ["obesidad",
-- "sinusitis"])
-- Raton "Orejudo" 4.0 9.0 ["sinusitis"]
-- > reduceFatFast 2 (Raton "Orejudo" 4.0 10.0 [“obesidad",
-- "sinusitis"])
-- Raton "Orejudo" 4.0 8.1 ["sinusitis"]

reduceFatFast :: Int -> Animal -> Animal
reduceFatFast potencia raton = medicamento (hierbaVerde "obesidad": take potencia (repetir alcachofa)) raton

reduceFatFast' potencia raton = medicamento (replicate potencia alcachofa ++ [hierbaVerde "obesidad"] ) raton

-- d. Hacer la función hierbaMilagrosa, que es un medicamento que usa hierbasVerdes
-- para curar todas las enfermedades infecciosas.