import Text.Show.Functions

-- Punto 1: (2 puntos) Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.

data Gema = UnaGema { nombreGema::String , poderGema::PoderGema } deriving Show

data Personaje = UnPersonaje { nombre::String, planeta::String, edad::Int, energia::Int, habilidades::[String] } deriving Show

data Guantelete = UnGuantelete { material::String, gemas::[Gema] } deriving Show

type Universo = [Personaje]

type Chasquido = Guantelete -> Universo -> Universo

ironMan = UnPersonaje "Iron Man" "tierra" 45 100 ["fuerza", "volar", "piel de metal"]
drStrange = UnPersonaje "Dr Strange" "tierra" 40 150 ["volar", "telepatía", "telekinesis"]
groot = UnPersonaje "Groot" "tierra" 200 80 ["piel de madera", "crecimiento vegetal"]
wolverine = UnPersonaje "Wolverine" "tierra" 55 200 ["regeneracion", "fuerza", "garras de metal"]
viudaNegra = UnPersonaje "Viuda Negra" "tierra" 30 150 ["artes marciales", "fuerza"]
unHumano = UnPersonaje "Marcos" "tierra" 30 50 ["programar", "programacion en Haskell", "usar Mjolnir"]

unUniverso :: Universo
unUniverso = [ironMan, drStrange, groot, wolverine, viudaNegra, unHumano]

gemasPosibles::[String]
gemasPosibles = ["mente", "alma", "espacio", "poder", "tiempo", "gema loca"]

tieneTodasLasGemas :: Guantelete -> Bool
tieneTodasLasGemas guantelete = (all (\g -> elem g  (map nombreGema.gemas $ guantelete))) $ gemasPosibles

esGuanteleteCompleto :: Guantelete -> Bool
esGuanteleteCompleto guantelete = ((== "uru").material $ guantelete) && (tieneTodasLasGemas guantelete) 

unChasquido :: Chasquido
unChasquido guantelete universo | esGuanteleteCompleto guantelete = take ((`div` 2).length $ universo) universo
                                | otherwise = undefined

-- Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.
-- a) Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.
esAptoPendex :: Universo -> Bool
esAptoPendex universo = any ((> 45).edad) universo

-- b) Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.
energiaTotalDelUniverso :: Universo -> Int
energiaTotalDelUniverso universo = sum.(map energia).(filter ((> 1).length.habilidades)) $ universo

-- Segunda parte
-- A su vez, aunque el guantelete no se encuentre completo con las 6 gemas, el poseedor puede utilizar el poder del mismo contra un enemigo, es decir que puede aplicar el poder de cada gema sobre el enemigo. Las gemas del infinito fueron originalmente parte de la entidad primordial llamada Némesis, un ser todopoderoso del universo anterior quién prefirió terminar su existencia en lugar de vivir como la única conciencia en el universo. Al morir, dio paso al universo actual, y el núcleo de su ser reencarnó en las seis gemas: 
-- La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.
-- El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. Además le quita 10 puntos de energía. 
-- El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.
-- El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).
-- El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, no puede dejar la edad del oponente con menos de 18 años. Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. Si tiene 45, le quedarán 22 (por división entera). Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.
-- La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.

-- Punto 3: (3 puntos) Implementar las gemas del infinito, evitando lógica duplicada. 

type PoderGema = Personaje -> Personaje

poderGemaMente :: Int -> PoderGema
poderGemaMente valor personaje = personaje { energia = (energia personaje) - valor }

poderGemaAlma ::  String -> PoderGema
poderGemaAlma habilidad personaje = (poderGemaMente 10).(\p -> p { habilidades = (filter (/= habilidad)).habilidades $ p}) $ personaje

poderGemaEspacio :: String -> PoderGema
poderGemaEspacio unPlaneta personaje = (poderGemaMente 20).(\p -> p { planeta = unPlaneta }) $ personaje

removerDosHabilidades :: PoderGema
removerDosHabilidades personaje | (<= 2).length.habilidades $ personaje = personaje { habilidades = [] }
                                | otherwise = personaje

poderGemaPoder :: PoderGema
poderGemaPoder personaje = (poderGemaMente (energia personaje)).removerDosHabilidades $ personaje

reducirEdadAMitad :: PoderGema
reducirEdadAMitad personaje | (>= 18).(`div` 2).edad $ personaje = personaje { edad = (`div` 2).edad $ personaje }
                            | otherwise = personaje { edad = 18 }

poderGemaTiempo :: PoderGema
poderGemaTiempo personaje = (poderGemaMente 50).reducirEdadAMitad $ personaje

poderGemaLoca :: (PoderGema) -> PoderGema
poderGemaLoca unPoderGema personaje = foldl (\x y -> y x) personaje (replicate 2 unPoderGema)

-- Punto 4: (1 punto) Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.

guanteleteDeGoma = UnGuantelete "goma" [UnaGema "alma" (poderGemaAlma "usar Mjolnir"), UnaGema "gema loca" (poderGemaLoca (poderGemaAlma "programacion en Haskell"))]
guanteleteDeCarton = UnGuantelete "carton" [UnaGema "alma" (poderGemaAlma "usar Mjolnir"), UnaGema "gema loca" (poderGemaLoca (poderGemaAlma "programacion en Haskell")), UnaGema "mente" (poderGemaMente 1000)]

-- Punto 5: (2 puntos). No se puede utilizar recursividad. Generar la función utilizar  que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas personaje = foldl (\p g -> ($ p).poderGema $ g) personaje gemas

-- Punto 6: (2 puntos). Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 

gemaMayor :: Personaje -> Gema -> Gema -> Gema
gemaMayor personaje unaGema otraGema    | (energia.($ personaje).poderGema $ unaGema) < (energia.($ personaje).poderGema $ otraGema) = unaGema
                                        | otherwise = otraGema

buscarGemaMasPoderosa :: Personaje -> [Gema] -> Gema -> Gema
buscarGemaMasPoderosa personaje [x] gemaAux = gemaMayor personaje x gemaAux
buscarGemaMasPoderosa personaje (x:xs) gemaAux = buscarGemaMasPoderosa personaje xs (gemaMayor personaje x gemaAux)

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = buscarGemaMasPoderosa personaje (gemas guantelete) (head.gemas $ guantelete)

-- Punto 7: (1 punto) Dada la función generadora de gemas y un guantelete de locos:
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

tiempo = UnaGema "tiempo" poderGemaTiempo

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas tiempo)

-- Y la función 
usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
-- gemaMasPoderosa punisher guanteleteDeLocos

{-
No puede utilizarse esta función ya que gemaMasPoderosa intentará buscar la gema cuyo poder genere un personaje con la menor cantidad de energía posible y al ser una lista infinita no podrá terminar nunca de concluir cuál es.
-}

-- usoLasTresPrimerasGemas guanteleteDeLocos punisher

{-
Esta invocación sí puede terminar ya que al tener un take 3 está acotando la lista infinita
-}
