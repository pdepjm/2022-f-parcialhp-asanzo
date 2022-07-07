
-- Nombre: Pdep, Cátedra
-- Legajo: [1..]

-- Parcial Jueves 09 de Junio de 2022
-- Paradigmas de Programación UTN.BA Jueves Mañana

module Library where
import PdePreludat

-- enunciado
-- https://docs.google.com/document/d/1jNjWDojVUCg_PtY3_0XLCbGYkEOP-Jus0_BRzGGI2_o/edit#

-- 1. Postres
-- Punto 1A)

data Postre = UnPostre { 
    sabores :: [String],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)

bizcocho :: Postre
bizcocho = UnPostre ["borracho", "fruta", "crema"] 100 20

-- Punto 1B)

type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio = perderPesoPorc 5 . calentar 1

immobulus :: Hechizo
immobulus postre = postre { temperatura = 0 }

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = perderPesoPorc 10 . agregarSabor "concentrado"

diffindo :: Porcentaje -> Hechizo
diffindo = perderPesoPorc

riddikulus :: String -> Hechizo
riddikulus sabor = agregarSabor (reverse sabor)

avadaKedabra :: Hechizo
avadaKedabra = immobulus . sacarSabores


type Porcentaje = Number
perderPesoPorc :: Porcentaje -> Postre -> Postre
perderPesoPorc porcentaje postre = postre { peso = peso postre * (100 - porcentaje) / 100 }
agregarPeso :: Number -> Postre -> Postre
agregarPeso cantidad postre = postre {peso = peso postre + cantidad}

calentar :: Number -> Postre -> Postre
calentar temp postre = postre { temperatura = temperatura postre + temp }

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre { sabores = sabor : sabores postre }
sacarSabores :: Postre -> Postre
sacarSabores postre = postre { sabores = [] }

-- Punto 1C)
estaCongelado :: Postre -> Bool
estaCongelado = (>0).temperatura

tieneAlgunSabor :: Postre -> Bool
tieneAlgunSabor = (>0).length.sabores 

estaListo :: Postre -> Bool
estaListo postre = (peso postre) > 0 && tieneAlgunSabor postre && not (estaCongelado postre)

estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo = all (estaListo.hechizo)  

-- Punto 1D)
promedio :: [Number] -> Number
promedio nums = sum nums / length nums 

pesoPromedio :: [Postre] -> Number
pesoPromedio = promedio . map peso . filter estaListo

-- 2. Magos

-- Punto 2A)

data Mago = UnMago {
    hechizos :: [Hechizo],
    cantHorrorcruxes :: Number
} deriving Show

-- Punto 2A)

practicar ::  Hechizo -> Postre -> Mago -> Mago
practicar hechizo postre = agregarHorrocruxSegun hechizo postre . aprender hechizo

aprender :: Hechizo -> Mago -> Mago 
aprender hechizo mago = mago{ hechizos = hechizo : hechizos mago } 

agregarHorrocruxSegun :: Hechizo -> Postre -> Mago -> Mago
agregarHorrocruxSegun hechizo postre mago
    | esEquivalenteAAvadaKedavra hechizo postre = sumarHorrocrux mago
    | otherwise = mago

esEquivalenteAAvadaKedavra :: Hechizo -> Postre -> Bool
esEquivalenteAAvadaKedavra hechizo postre = hechizo postre == avadaKedabra postre

sumarHorrocrux :: Mago -> Mago
sumarHorrocrux mago = mago {cantHorrorcruxes = cantHorrorcruxes mago + 1}

-- 2C)

mejorHechizoV1 :: Postre -> Mago -> Hechizo
mejorHechizoV1 postre mago = elMejor postre (hechizos mago)

elMejor :: Postre -> [Hechizo] -> Hechizo
elMejor postre [hechizo] = hechizo
elMejor postre (primer:segundo:restohechizos) | esMejor postre primer segundo = elMejor postre (primer:restohechizos)
    | otherwise = elMejor postre (segundo:restohechizos)

esMejor :: Postre -> Hechizo -> Hechizo -> Bool
esMejor postre hechizo1 hechizo2 = (length . sabores . hechizo1) postre > (length . sabores . hechizo2) postre

-- Otra versión
mejorhechizoV2 ::  Postre -> Mago -> Hechizo
mejorhechizoV2 postre mago = foldl1 (elMejorEntre postre) (hechizos mago)

elMejorEntre :: Postre -> Hechizo -> Hechizo -> Hechizo
elMejorEntre postre hechizo1 hechizo2 
    | esMejor postre hechizo1 hechizo2 = hechizo1
    | otherwise = hechizo2

-- Otra versión más
mejorhechizoV3 ::  Postre -> Mago -> Hechizo
mejorhechizoV3 postre mago = mejorGenerico (\hechizo -> (length.sabores.hechizo) postre) (hechizos mago)

mejorGenerico :: Ord a => (b -> a) ->[b] -> b
mejorGenerico criterio elementos = foldl1 (elMejorEntreGenerico criterio) (elementos)

elMejorEntreGenerico :: Ord a => (b -> a) -> b -> b -> b
elMejorEntreGenerico criterio elemento1 elemento2
    | criterio elemento1 > criterio elemento2 = elemento1
    | otherwise = elemento2

-- Punto 3 Infinita Magia
-- Punto 3A)

mesaInfinita :: [Postre]
mesaInfinita = repeat bizcocho

magoInf :: Receta
magoInf = UnMago { hechizos = repeat avadaKedabra, cantHorrorcruxes = 0}

-- Punto 3B)
{-
Verdadero, existe la consulta:
Prelude> estanListos avadaKedabra mesaInfinita

La ejecución devuelve falso pues debido a la evaluación diferida, el all cuando encuentra el primer postre que no está listo ya retorna y no requiere construir la lista infinita.
-}

-- Punto 3C)
{-
No existe ninguna forma de conocer el mejor hechizo del mago porque para hacerlo hay que evaluar todos los elementos lista, aún teniendo lazy evaluation.
-}
