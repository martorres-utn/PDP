-- Entrevistando por un sueño
 
-- Una consultora de RRHH, además de agregarnos a LinkedIn, nos pidió que le hagamos un sistema que los ayude con sus tareas habituales.
-- La idea es básicamente saber si alguien es apto para un puesto.
-- De cada postulante se conoce su nombre, su edad, su remuneración pretendida y una lista de conocimientos.

data Postulante = UnPostulante {nombre :: String, edad :: Int, remuneracion :: Float, conocimientos :: [String]} | EstudianteUTN { legajo::String, conocimientoUTN::[String] } deriving Show
 
pepe = UnPostulante "Jose Perez" 35 15000.0 ["Haskell", "Prolog", "Wollok", "C", "repetir logica"]
tito = UnPostulante "Roberto González" 20 12000.0 ["Haskell", "Php"]
 
-- También hay algunos puestos a cubrir, con sus correspondientes conocimientos necesarios:
data Puesto = UnPuesto { nombrePuesto::String, requisitos::[String] }
jefe = UnPuesto "gerente de sistemas" ["Haskell", "Prolog", "Wollok"]
chePibe = UnPuesto "cadete" ["ir al banco"]
 
apellidoDueno::String
apellidoDueno = "González"
 
-- 1)    Recursos humanos
-- La empresa tiene una política de recursos humanos que se basa en los siguientes requisitos:
-- a.    tieneConocimientos recibe un puesto y un postulante y devuelve si el postulante posee todos los conocimientos requeridos para el puesto.

tieneConocimientos :: Puesto -> Postulante -> Bool
tieneConocimientos puesto postulante = ((all (\r -> elem r (conocimientos postulante) ) ).requisitos) puesto

-- b.    edadAceptable recibe una edad mínima y una edad máxima y un postulante y devuelve si el postulante tiene la edad requerida para el puesto.

edadAceptable :: Int -> Int -> Postulante -> Bool
edadAceptable edadMin edadMax postulante = ((\x -> x >= edadMin && x <= edadMax).edad) postulante

-- c.     sinArreglo recibe a un postulante y verifica que no tenga el apellido del dueño de la empresa. (se asume que el apellido es el último nombre del postulante)

sinArreglo :: Postulante -> Bool
sinArreglo postulante = (not.(== apellidoDueno).last.words.nombre) postulante

-- 2)    Preselección
-- En base a lo anterior, y previendo la existencia de nuevos criterios, definir una función que recibe una lista de postulantes y una lista con los requisitos de selección que devuelva a los postulantes que cumplen con todo lo indicado.
-- a.    Mostrar un ejemplo de consultas para el puesto de gerente de sistemas, con edad entre 30 y 40 años, con los conocimientos correspondientes y que no esté arreglado.
-- b.    Sin definir una nueva función, añadir a la consulta anterior, un nuevo requisito, que consiste en que entre los conocimientos del postulante no esté "repetir lógica".
 
preseleccion :: [Postulante] -> [(Postulante -> Bool)] -> [Postulante]
preseleccion postulantes requerimientos = filter (\p -> all (\r -> r p) requerimientos) postulantes

-- 3)    Esperando que te llamen
-- Representar el paso de un año de espera entre los postulantes. Sabiendo que existe una función incrementarEdad que incrementa en 1 la edad del postulante y una función aumentarSueldo que recibe un porcentaje y un postulante y le aumenta en ese porcentaje el sueldo que pretende cobrar, y sin hacer nuevas funciones auxiliares, definir la función actualizarPostulantes que reciba una lista con postulantes y, que además de aumentar la edad a todos, actualice sus sueldos pretendidos en un 27%. Realizarlo de dos maneras:
-- a.    Utilizando listas por comprensión.
-- b.    Utilizando composición y aplicación parcial.
-- ¿Cual de las dos soluciones preferís? ¿por qué?
-- ¿Qué sucede si la lista de postulantes es infinita? Justificar. Mostrar un ejemplo de consulta donde suceda lo indicado.
 
incrementarEdad :: Postulante -> Postulante
incrementarEdad postulante = postulante { edad = edad postulante + 1 }

aumentarSueldo :: Float -> Postulante -> Postulante
aumentarSueldo porcentaje postulante = postulante { remuneracion = remuneracion postulante * (1.0 + porcentaje / 100.0) }

infinitosPostulantes = [tito] ++ infinitosPostulantes

actualizarPostulantes :: [Postulante] -> [Postulante]
actualizarPostulantes postulantes = map ((aumentarSueldo 27).incrementarEdad) postulantes

actualizarPostulantes' :: [Postulante] -> [Postulante]
actualizarPostulantes' postulantes = [ ((aumentarSueldo 27).incrementarEdad) x | x <- postulantes ]

-- sin el take se cuelga todo (No termina)

-- *Main> take 3 (actualizarPostulantes infinitosPostulantes)
-- [UnPostulante {nombre = "Roberto Gonz\225lez", edad = 21, remuneracion = 15240.0, conocimientos = ["Haskell","Php"]},UnPostulante {nombre = "Roberto Gonz\225lez", edad = 21, remuneracion = 15240.0, conocimientos = ["Haskell","Php"]},UnPostulante {nombre = "Roberto Gonz\225lez", edad = 21, remuneracion = 15240.0, conocimientos = ["Haskell","Php"]}]
-- *Main> take 2 (actualizarPostulantes infinitosPostulantes)
-- [UnPostulante {nombre = "Roberto Gonz\225lez", edad = 21, remuneracion = 15240.0, conocimientos = ["Haskell","Php"]},UnPostulante {nombre = "Roberto Gonz\225lez", edad = 21, remuneracion = 15240.0, conocimientos = ["Haskell","Php"]}]

-- 4)    Capacitaciones
-- La empresa decide comenzar a hacer capacitaciones a los postulantes, pero además de los que se presentaron, convoca a los alumnos de la UTN anotados en la bolsa de trabajo, para que también se nutran y en un futuro entren a la empresa.
-- a.    Definir la función capacitar que dado un postulante y un conocimiento, lo agregue a su lista de conocimientos.

capacitar :: Postulante -> String -> Postulante
capacitar postulante unConocimiento = postulante { conocimientos = (conocimientos postulante) ++ [unConocimiento] }

capacitar' :: Postulante -> String -> Postulante
capacitar' postulante unConocimiento = postulante { conocimientoUTN = [unConocimiento] ++ (conocimientoUTN postulante) }


-- b.    Definir la misma función pero para un estudiante, del cual sólo se conoce el legajo y sus conocimientos, y que cuando se lo quiere capacitar, aprende el nuevo conocimiento, pero se olvida del último.
-- data Estudiante = EstudianteUTN { legajo::String, conocimientoUTN::[String] } deriving Show
pili = EstudianteUTN "100200-3" ["C++", "VB", "Haskell"]

capacitarEstudiante :: Postulante -> String -> Postulante 
capacitarEstudiante estudiante unConocimiento = (\e -> e { conocimientoUTN = take (length (conocimientoUTN e) - 1) (conocimientoUTN e) } ) (capacitar' estudiante unConocimiento) --{ conocimientoUTN = take (length (conocimientoUTN estudiante) - 1) (conocimientoUTN estudiante)}
-- La profesora en clase armó una función con guardas que se comporta de una forma u otra dependiendo si es un Estudiante o un Postulante.

-- capacitarU :: Postulante -> String -> Postulante
-- capacitarU (UnPostulante _ _ _ conocimientoPostulante) unConocimiento = capacitar unConocimiento
-- capacitarU (EstudianteUTN _ conocimientoEstudiante) = capacitarEstudiante unConocimiento


-- c.     Hacer una función capacitación, que reciba un puesto y una persona, ya sea estudiante o postulante, y lo capacite en todos los conocimientos necesarios para dicho puesto. Definir su tipo y mostrar al menos dos ejemplos de invocación.
-- d.    ¿Qué más habría que definir para que se pueda hacer una preselección entre los estudiantes? Justificar
