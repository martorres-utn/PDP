{-
 1) Queremos calcular el sueldo de los empleados de nuestra empresa. Tenemos dos tipos de empleado:
- Los comunes: nos interesa saber el sueldo básico y el nombre.
- Los jerárquicos: nos interesa conocer el sueldo básico, la cantidad de gente a cargo y el nombre.
 
El sueldo que cobran los comunes se determina por el sueldo básico, en los empleados  jerárquicos se calcula como sueldo básico + plus por la cantidad de gente a cargo (500 por cada persona a cargo).
 
> sueldo( Jefe 15000 3 “Sonia”)
16500
 -}
data Empleado = Comun { nombre::String, sueldoBasico::Double } | 
                Jefe { nombre::String, sueldoBasico::Double, cantidadACargo::Integer }
calcularSueldo :: Empleado -> Double
calcularSueldo (Comun nombre sueldoBasico) = sueldoBasico
calcularSueldo (Jefe nombre sueldoBasico cantidadACargo) = sueldoBasico + fromInteger (calcularPlus cantidadACargo)

calcularPlus cantidad = cantidad * 500

{- 
2) Se conocen estas bebidas:
 
data Bebida = Cafe Nombre Azucar | Gaseosa Sabor Azucar

Dado un producto determinar si es energizante.
-      Si es café es energizante si es un capuchino.
-   Si es una gaseosa es energizante si es de sabor a pomelo y tiene más de 10gr de azúcar.
>esEnergizante (Gaseosa “pomelo” 12)
True
 -}

data Bebida = Cafe { nombreBebida::String, azucar::Double } | Gaseosa { sabor::String, azucar::Double }
esEnergizante :: Bebida -> Bool
esEnergizante (Cafe "capuchino" _) = True
esEnergizante (Gaseosa "pomelo" azucar) = azucar > 10
esEnergizante _ = False

{- 
3) Resolver la función find’ que encuentra el primer elemento que cumple una condición. No se puede resolver con recursividad. Si ningún elemento cumple la condición dejar que falle.
find’ :: (a -> Bool) -> [a] -> a
 *Main> find' even [41..339]
 42
 -}

find' :: (a -> Bool) -> [a] -> a
find' criterio lista = head [elemento | elemento <- lista, criterio elemento]

find'' :: (a -> Bool) -> [a] -> a
find'' criterio lista = (head.filter criterio) lista

{- 
3.1) Aprovechar la función find’ para aplicarla a este dominio.
data Politico = Politico {proyectosPresentados :: [String], sueldo :: Float, edad :: Int } deriving Show
 
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]
 
Queremos encontrar:
a)  un político joven (menos de 50 años)
b)  alguno que haya presentado más de 3 proyectos
c) alguno que haya presentado algún proyecto que tenga más de 3 palabras
No hay que generar funciones, sino aprovechar find’ y desde la consola resuelva los tres requerimientos.
 -}
data Politico = Politico {proyectosPresentados :: [String], sueldo :: Float, edad :: Int } deriving Show 
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]

-- a) find' (< 50.edad) politicos