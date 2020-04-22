data Empleado = Comun { nombre::String, sueldoBasico::Double } | 
                Jefe { nombre::String, sueldoBasico::Double, cantidadACargo::Integer }
calcularSueldo :: Empleado -> Double
calcularSueldo (Comun nombre sueldoBasico) = sueldoBasico
calcularSueldo (Jefe nombre sueldoBasico cantidadACargo) = sueldoBasico + fromInteger (calcularPlus cantidadACargo)

calcularPlus cantidad = cantidad * 500

-- data Cafe = { nombre::String, azucar::Double }
-- data Gaseosa = { sabor::String, azucar::Double }
data Bebida = Cafe { nombreBebida::String, azucar::Double } | Gaseosa { sabor::String, azucar::Double }

esEnergizante :: Bebida -> Bool
esEnergizante (Cafe "capuchino" _) = True
esEnergizante (Gaseosa "pomelo" azucar) = azucar > 10
esEnergizante _ = False