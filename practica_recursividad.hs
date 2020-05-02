{- 1. Definir la función promediosAlumnos/1, que dada una lista de alumnos devuelve una lista de tuplas que tenga el alumno y el promedio (Consideramos la división entera para el promedio y usamos la funcion div).
type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre = Nombre, notas = Notas}
Main> promediosAlumnos[(Alumno "juan" [8,6]), (Alumno "maria" [7,9,4]), (Alumno "ana" [6,2,4])]
[("juan",7),("maria",6),("ana",4)]
 -}

type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre::Nombre, notas::Notas} deriving Show

promedioAlumno :: Persona -> (Nombre, Int)
promedioAlumno (Alumno nombre notas) = (nombre, div (sum notas) (length notas) ) 

promediosAlumnos :: [Persona] -> [(Nombre, Int)]
promediosAlumnos [] = []
promediosAlumnos (x:xs) = [promedioAlumno x] ++ promediosAlumnos xs

promediosAlumnos' :: [Persona] -> [(Nombre, Int)]
promediosAlumnos' alumnos = map (\e -> (nombre e, (promedio . notas) e)) alumnos

{- 
2. Definir la función promediosSinAplazos/1, que dada una lista de listas, devuelve la lista de los promedios de cada lista-elemento, excluyendo los que sean menores a 6 que no se cuentan.
Main> promediosSinAplazos [[8,6],[6,6,4]]
[7,6]
 -}
promedio :: Notas -> Int
promedio notas = div (sum notas) (length notas)

promediosSinAplazos :: [Notas] -> [Int]
promediosSinAplazos listaNotas = map (promedio.filter (>=6)) listaNotas 

promediosSinAplazos' :: [[Int]] -> [[Int]]
promediosSinAplazos' lista = filter (\elem -> (div (sum elem) (length elem)) > 5 ) lista

-- 3. Definir la función aprobó/1, que dado un alumno devuelve True si el alumno aprobó. Aclaración: se dice que un alumno aprobó si todas sus notas son 6 o más.
-- Main> aprobo (Alumno “manuel” [8,6,2,4])
-- False

aprobo :: Persona -> Bool
aprobo alumno = ((all (>5)). notas) alumno

-- 4. Definir la función aprobaron/1, que dada una lista de alumnos, devuelve los nombres de los alumnos que aprobaron.
-- Main>aprobaron [Alumno “manuel” [8,6,2,4] , Alumno “elena” [7,9,8,7], Alumno “ana” [6,2,4,2], Alumno “pedro” [9,6,7,10]]
-- [“elena”, “pedro”]

aprobaron :: [Persona] -> [Nombre]
aprobaron alumnos = ((map nombre) . (filter aprobo)) alumnos