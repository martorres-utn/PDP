-- Una popular empresa de streaming de series y películas decidió hacer más
-- funcional su sistema y para ello solicitó la ayuda de los expertos estudiantes de la
-- Universidad.

-- Primera parte: Usuarios cinéfilos

-- El sistema maneja básicamente información de usuarios y películas. De las películas
-- se conoce su nombre, género, duración y origen. De los usuarios se conoce su nombre,
-- edad, categoría (básica, estándar o premium), país de residencia, las películas vistas por él
-- y el estado de salud.

-- 1) Modelar los tipos de datos necesarios

data Media = Pelicula { titulo::String, duracion::Int, genero::String, origen::String } deriving (Show, Eq)

data User = Usuario { nombre::String, categoria::String, edad::Int, paisResidencia::String, peliculasVistas::[Media], estadoSalud::Int } deriving Show

psicosis = (Pelicula "Psicosis" 109 "Terror" "Estados Unidos")

perfumeDeMujer = (Pelicula "Perfume de Mujer" 150 "Drama" "Estados Unidos")

elSaborDeLasCervezas = (Pelicula "El sabor de las cervezas" 95 "Drama" "Iran")

lasTortugasTambienVuelan = (Pelicula "Las tortugas también vuelan" 103 "Drama" "Iran")

eraserHead = (Pelicula "Eraserhead" 100 "Terror" "Estados Unidos")

juan = (Usuario "juan" "estandar" 23 "Argentina" [perfumeDeMujer] 60)

marcos = (Usuario "marcos" "basica" 30 "Argentina" (replicate 21 elSaborDeLasCervezas) 10)

-- 2) Hacer la función que permita que un usuario pueda ver una película, lo que implica
-- que se agregue a su lista de películas vistas.
-- Por ejemplo
-- > ver psicosis juan
-- Usuario "juan" "estandar" 23 [ Pelicula "El pianista" 150
-- "Drama" “Alemania”, Pelicula "Psicosis" 109 "Terror" "Estados
-- Unidos" ] 60

ver :: Media -> User -> User
ver pelicula usuario = usuario { peliculasVistas = (peliculasVistas usuario) ++ [pelicula] }

-- ver' :: Pelicula -> Usuario -> Usuario
-- ver' pelicula usuario = usuario { peliculasVistas = (peliculasVistas usuario) ++ [pelicula] }

-- 3) Se necesita premiar a los usuarios internacionales fieles. De esta manera, a todos
-- aquellos que hayan visto más de 20 películas que no sean de Estados Unidos, se
-- les sube de categoría (el premium la mantiene). La función debe recibir una lista de
-- usuarios y devolver una nueva lista.

esUsuarioFiel :: User -> Bool
esUsuarioFiel usuario = (length.filter (\p -> (origen p) /= "Estados Unidos").peliculasVistas) usuario > 20

subirCategoria :: User -> User
subirCategoria usuario  | categoria usuario == "basica" = usuario { categoria = "estandard" }
                        | categoria usuario == "estandard" = usuario { categoria = "premium" }
                        | otherwise = usuario

premiarUnUsuarioFiel :: User -> User
premiarUnUsuarioFiel usuario    | esUsuarioFiel usuario = subirCategoria usuario
                                | otherwise = usuario

premiarUsuariosFieles :: [User] -> [User]
premiarUsuariosFieles usuarios = map premiarUnUsuarioFiel usuarios

-- 4) Una de las
-- características
-- más
-- importante del
-- sistema es
-- poder buscar
-- películas, por
-- lo que existen
-- distintos

-- criterios de búsqueda. Implementar los siguientes:
-- ● Te quedaste corto: se considera corto aquella película cuya duración es
-- menor a 35 minutos
-- ● Cuestión de género: cuando la película es de alguno de los géneros
-- indicados.
-- ● De donde saliste: se buscan las películas de un origen en particular.
-- ● Va por ese lado: definir un criterio de búsqueda que se base en la similitud
-- con respecto de otra película en base a una característica de la misma como
-- puede ser: el género, el origen, etc.

type CriterioBusqueda = Media -> Bool

buscarPelicula :: (CriterioBusqueda) -> [Media] -> [Media]
buscarPelicula criterio lista = filter criterio lista

teQuedasteCorto :: CriterioBusqueda
teQuedasteCorto pelicula = ((< 35).duracion) pelicula

cuestionDeGenero :: [String] -> CriterioBusqueda
cuestionDeGenero generos pelicula = ((flip elem) generos).genero $ pelicula 

deDondeSaliste :: String -> CriterioBusqueda
deDondeSaliste paisOrigen pelicula = origen pelicula == paisOrigen

vaPorEseLado :: (Eq b) => (Media -> b) -> Media -> CriterioBusqueda
vaPorEseLado caracteristica peliculaA peliculaB = caracteristica peliculaA == caracteristica peliculaB

-- 5) Hacer una función que a partir de un usuario y un conjunto de criterios de búsqueda
-- y teniendo en cuenta la base de películas de la empresa, proponga tres películas
-- que cumplan simultáneamente con los criterios indicados y que no hayan sido vistas
-- por el usuario. Mostrar un ejemplo de invocación y respuesta para una persona que
-- quiere ver una película iraní, que sea drama o comedia. Y además, que no sea un
-- corto.

basePeliculas = [psicosis, perfumeDeMujer, elSaborDeLasCervezas, lasTortugasTambienVuelan, eraserHead]

cumpleTodosLosCriterios :: [CriterioBusqueda] -> Media -> Bool
cumpleTodosLosCriterios criterios pelicula = all (\c -> c pelicula) criterios

usuarioVioPelicula :: User -> Media -> Bool
usuarioVioPelicula usuario pelicula = (elem pelicula).peliculasVistas $ usuario

sugerirPeliculas :: User -> [CriterioBusqueda] -> [Media] -> [Media]
sugerirPeliculas usuario criterios dbPeliculas = take 3.filter (\p -> cumpleTodosLosCriterios criterios p && (not.usuarioVioPelicula usuario) p ) $ dbPeliculas

-- Segunda parte: Maratón de series
-- La gente de la empresa incorpora series. A su vez, está preocupada por la salud emocional
-- de sus usuarios, ante los interminables maratones de series que suelen tener.

-- 1) Definir el tipo de dato para un capítulo de una serie, sabiendo que de cada uno se
-- tiene la misma información que una película, pero además se cuenta con una
-- determinada forma en la que altera la salud del usuario.

type EfectoUsuario = User -> User

data CapituloSerie = UnCapituloSerie { informacion::Media , efecto::EfectoUsuario }

-- 2) Asumiendo que los usuarios no ven sino consumen series, hacer la función que
-- recibiendo al usuario y un capítulo de la serie, en vez de registrarlo como vista,
-- devuelva cómo queda la persona.

consumeSerie :: User -> CapituloSerie -> User
consumeSerie usuario capitulo = (efecto capitulo) usuario

afectaSalud :: (Int -> Int) -> EfectoUsuario
afectaSalud efectoSalud usuario = usuario { estadoSalud = (efectoSalud.estadoSalud) usuario }

-- 3) Mostrar un ejemplo, inventando una forma de alterar la salud del usuario.
-- 4) Hacer una función llamada maraton, que recibiendo un usuario y una serie completa,
-- devuelva cómo queda la persona luego de consumir todos sus capítulos.

type SerieCompleta = [CapituloSerie]

community :: SerieCompleta
community = [(UnCapituloSerie (Pelicula "community capitulo 1" 20 "Comedia" "EstadosUnidos") (afectaSalud (+30))), (UnCapituloSerie (Pelicula "community capitulo 2" 20 "Comedia" "EstadosUnidos") (afectaSalud (+40)))]

maraton :: User -> SerieCompleta -> User
maraton usuario serieCompleta = foldl (\u e -> e u) usuario (map (\c -> efecto c) serieCompleta)

maraton' :: User -> SerieCompleta -> User
maraton' usuario serieCompleta = foldl consumeSerie usuario serieCompleta


-- 5) ¿Qué sucedería si la serie tuviera una cantidad infinita de capítulos?
-- si se invocara maraton con una serie de infinitos capitulos nunca podríamos conocer el estado final del usuario porque no se podría resolver.

-- 6) ¿Como se haría para representar que un usuario realiza un maratón de una serie
-- con una cantidad indeterminada de capítulos, pero indicando cuantos capítulos se
-- desea considerar?

maratonLimitada :: Int -> User -> SerieCompleta -> User
maratonLimitada limite usuario serieCompleta = foldl (\u e -> e u) usuario (map (\c -> efecto c) (take limite serieCompleta))