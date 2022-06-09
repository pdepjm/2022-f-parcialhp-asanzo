
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

aguamenti :: Number -> Hechizo
aguamenti cantidadAgua = agregarPeso cantidadAgua . agregarSabor "suave"

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

-- 2 Magos recetas

-- Punto 2A)

type Receta = [Hechizo]

data Mago = UnMago {
    recetas :: [Receta],
    cantHorrorcruxes :: Number
}

-- Cocinar un postre con una receta (se espera obtener el postre luego de todos los hechizos)

cocinar :: Postre -> Receta -> Postre
cocinar postre hechizos = (foldr1 (.) hechizos) postre

-- Otra versión pensando en los valores en lugar de en las funciones:
-- cocinar postre hechizos = foldr ($) postre hechizos

-- Punto 2B)

trabajar ::  Receta -> Postre -> Mago -> Mago
trabajar receta postre = agregarHorrocruxSegun receta postre . aprenderReceta receta

aprenderReceta :: Receta -> Mago -> Mago 
aprenderReceta receta mago = mago{ recetas = receta : recetas mago } 

agregarHorrocruxSegun :: Receta -> Postre -> Mago -> Mago
agregarHorrocruxSegun receta postre mago
    | esEquivalenteAAvadaKedavra receta postre = sumarHorrocrux mago
    | otherwise = mago

esEquivalenteAAvadaKedavra :: Receta -> Postre -> Bool
esEquivalenteAAvadaKedavra receta postre = (cocinar postre receta) == (avadaKedabra postre)

sumarHorrocrux :: Mago -> Mago
sumarHorrocrux mago = mago {cantHorrorcruxes = cantHorrorcruxes mago + 1}

-- 2C)
puedeSuavizar :: Mago -> Postre -> Bool
puedeSuavizar mago postre = noTieneHorrorcruxes mago && algunaSuaviza postre (recetas mago)

noTieneHorrorcruxes :: Mago -> Bool
noTieneHorrorcruxes = (==0) . cantHorrorcruxes

algunaSuaviza :: Postre -> [Receta] -> Bool
algunaSuaviza postre = any (elem "suave" . sabores . cocinar postre) 

-- 2D)
mejorRecetaV1 :: Postre -> Mago -> Receta
mejorRecetaV1 postre mago = laMejor postre (recetas mago)

laMejor :: Postre -> [Receta] -> Receta
laMejor postre [receta] = receta
laMejor postre (primer:segunda:restoRecetas | esMejor postre primera segunda = laMejor postre (primera:restoRecetas)
    | otherwise = laMejor postre (segunda:restoRecetas)

esMejor :: Postre -> Receta -> Receta -> Bool
esMejor postre primera segunda = (length . sabores . cocinar postre) primera > (length . sabores . cocinar postre) segunda

-- Otra versión
mejorRecetaV2 ::  Postre -> Mago -> Receta
mejorRecetaV2 postre mago = foldl1 (laMejorEntre postre) (recetas mago)

laMejorEntre :: Postre -> Receta -> Receta -> Receta
laMejorEntre postre receta1 receta2 | esMejor postre receta1 receta2 = receta1
    | otherwise = receta2


