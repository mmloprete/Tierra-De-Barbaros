import Text.Show.Functions()
import Data.Char (toUpper)

type Objeto = Barbaro -> Barbaro

data Barbaro = UnBarbaro {
    nombre      :: String,
    fuerza      :: Int,
    habilidades :: [String],
    objetos     :: [Objeto]
} deriving Show

{-Barbaros-}
dave :: Barbaro
dave = UnBarbaro {
    nombre      = "Dave",
    fuerza      = 100,
    habilidades = ["tejer", "EscribirPoesia"],
    objetos     = [megafono, varitasDefectuosas]
}

faffy :: Barbaro
faffy = UnBarbaro {
    nombre      = "Faffy",
    fuerza      = 40,
    habilidades = ["robar", "Escribir Poesia Atroz"],
    objetos     = []
}

astro :: Barbaro
astro = UnBarbaro {
    nombre      = "Astro",
    fuerza      = 100,
    habilidades = ["Amortiguar", "Escribir Poesia Atroz"],
    objetos     = []
}

{-Barbaros-}

{-Mappers-}
cambiarNombre :: (String -> String) -> Barbaro -> Barbaro
cambiarNombre fn barbaro = barbaro {nombre = fn . nombre $ barbaro}

cambiarFuerza :: (Int -> Int) -> Barbaro -> Barbaro
cambiarFuerza fn barbaro = barbaro {fuerza = fn . fuerza $ barbaro}

cambiarHabilidades :: ([String] -> [String]) -> Barbaro -> Barbaro
cambiarHabilidades fn barbaro = barbaro {habilidades = fn . habilidades $ barbaro}
{-Mappers-}

{-Punto 1-}
espadas :: Int -> Objeto
espadas kilos barbaro = cambiarFuerza (+2*kilos) barbaro

amuletosMisticos :: String -> Objeto
amuletosMisticos habilidad barbaro = cambiarHabilidades (habilidad:) barbaro

varitasDefectuosas :: Objeto
varitasDefectuosas barbaro = barbaro {habilidades = ["hacerMagia"]}

ardilla :: Objeto
ardilla barbaro = barbaro

cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto1 objeto2 = objeto2.objeto1
{-Punto 1-}

{-Punto 2-}
megafono :: Objeto
megafono barbaro = cambiarHabilidades concatMayusHabilidades barbaro

concatMayusHabilidades :: [String] -> [String]
concatMayusHabilidades habilidades = ponerEnMayusculas.concatenarHabilidades $ habilidades

concatenarHabilidades :: [String] -> [String]
concatenarHabilidades habilidades = [concat habilidades]

ponerEnMayusculas :: [String] -> [String]
ponerEnMayusculas habilidades = map (map toUpper) habilidades

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono
{-Punto 2-}

{-Punto 3-}
type Evento = Barbaro -> Bool

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes barbaro = "Escribir Poesia Atroz" `elem` habilidades barbaro

cremalleraDelTiempo :: Evento
cremalleraDelTiempo barbaro = nombre barbaro == "Faffy" || nombre barbaro == "Astro"

saqueo :: Evento
saqueo barbaro = "robar" `elem` habilidades barbaro &&  fuerza barbaro > 80

gritoDeGuerra :: Evento
gritoDeGuerra barbaro = obtenerGritoDeGuerra barbaro > length (objetos barbaro) * 4

obtenerGritoDeGuerra :: Barbaro -> Int
obtenerGritoDeGuerra barbaro = length (concat.habilidades $ barbaro)

caligrafia :: Evento
caligrafia barbaro = habilidadesConMasDeTresVocales barbaro && habilidadesComienzanConMayuscula barbaro

habilidadesConMasDeTresVocales :: Barbaro -> Bool
habilidadesConMasDeTresVocales barbaro = all (habilidadMasDeTresVocales) (habilidades barbaro)

habilidadMasDeTresVocales :: String -> Bool
habilidadMasDeTresVocales habilidad = length (filtrarVocales habilidad) > 3

filtrarVocales :: String -> String
filtrarVocales habilidad = filter isVowel habilidad

isVowel :: Char -> Bool
isVowel caracter = caracter `elem` "aeiouAEIOU" 

habilidadesComienzanConMayuscula :: Barbaro -> Bool
habilidadesComienzanConMayuscula barbaro = all (habilidadComienzaConMayuscula) (habilidades barbaro)

habilidadComienzaConMayuscula :: String -> Bool
habilidadComienzaConMayuscula habilidad = head habilidad `elem` "ABCDEFGHIJKLDMNOPQRSTUVWXYZ"

type Aventura = [Evento]

ritualDeFechorias :: [Evento] -> Evento
ritualDeFechorias eventos barbaro = pasaUnaAventura any barbaro eventos

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes barbaros eventos = filter (\barbaro -> pasaUnaAventura all barbaro eventos ) barbaros

pasaUnaAventura :: (((t1 -> t2) -> t2) -> t3 -> t4) -> t1 -> t3 -> t4
pasaUnaAventura criterio barbaro eventos = criterio (\evento -> evento barbaro) eventos
{-Punto 3-}

{-Punto 4-}
sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (cabeza:cola) 
    | elem cabeza cola = cola
    | otherwise = (cabeza:cola)

descendiente :: Barbaro -> Barbaro
descendiente = utilizarObjetos.cambiarHabilidades sinRepetidos.cambiarNombre (++ "*") 

utilizarObjetos :: Barbaro -> Barbaro
utilizarObjetos barbaro = foldr ($) barbaro (objetos barbaro)

descendientes :: Barbaro -> [Barbaro]
descendientes barbaro = iterate descendiente barbaro

{-Se podrían aplicar sinRepetidos solo al nombre del barbaro 
ya que los objetos no son comparables entre si por ser funciones-}
{-Punto 4-}