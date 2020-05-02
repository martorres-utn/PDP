{- 1. Definir la función promediosAlumnos/1, que dada una lista de alumnos devuelve una lista de tuplas que tenga el alumno y el promedio (Consideramos la división entera para el promedio y usamos la funcion div).
type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre = Nombre, notas = Notas}
Main> promediosAlumnos[(Alumno "juan" [8,6]), (Alumno "maria" [7,9,4]), (Alumno "ana" [6,2,4])]
[("juan",7),("maria",6),("ana",4)]
 -}

type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre::Nombre, notas::Notas}

promedioAlumno :: Persona -> (Nombre, Int)
promedioAlumno (Alumno nombre notas) = (nombre, div (sum notas) (length notas) ) 

promediosAlumnos :: [Persona] -> [(Nombre, Int)]
promediosAlumnos [] = []
promediosAlumnos (x:xs) = [promedioAlumno x] ++ promediosAlumnos xs

{- 
2. Definir la función promediosSinAplazos/1, que dada una lista de listas, devuelve la lista de los promedios de cada lista-elemento, excluyendo los que sean menores a 6 que no se cuentan.
Main> promediosSinAplazos [[8,6],[6,6,4]]
[7,6]
 -}
promedio :: Notas -> Int
promedio notas = div (sum notas) (length notas)

promediosSinAplazos :: [Notas] -> [Int]
promediosSinAplazos  = map (promedio.filter (>=6)) 

promediosSinAplazos' :: [[Int]] -> [[Int]]
promediosSinAplazos' lista = filter (\elem -> (div (sum elem) (length elem)) > 5 ) lista