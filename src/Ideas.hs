module Ideas where
import PdePreludat
{-
-- Aguamenti: Agrega una cantidad de agua al postre, lo que hace que suba su peso en esa cantidad. Además, se le agrega el sabor “suave”.
aguamenti :: Number -> Hechizo
aguamenti cantidadAgua = agregarPeso cantidadAgua . agregarSabor "suave"

-- Saber si un mago puede suavizar un postre. Un mago puede suavizar un postre si no tiene horrorcruxes y además alguna receta de las que aprendió, al cocinarla, hace que el postre quede con el sabor “suave”.
puedeSuavizar :: Mago -> Postre -> Bool
puedeSuavizar mago postre = noTieneHorrorcruxes mago && algunaSuaviza postre (recetas mago)

noTieneHorrorcruxes :: Mago -> Bool
noTieneHorrorcruxes = (==0) . cantHorrorcruxes

algunaSuaviza :: Postre -> [Receta] -> Bool
algunaSuaviza postre = any (elem "suave" . sabores . cocinar postre) 

-- 2D)

data Mago = Mago {
    nombre::String,
    horrocruxes:: [Horrocrux]

}

data Horrocrux = Horrocrux {
    denominacion::String,
    destruido::Bool,
    mago::Mago
} 


diadema = Horrocrux {
    denominacion = "Ravenclow",
    destruido = False,
    mago = srTenebroso
}
harry = Horrocrux {
    denominacion = "Harry Postre",
    destruido = False,
    mago = srTenebroso
}
srTenebroso = Mago {
    nombre = "Voldemort",
    horrocruxes = [diadema,harry]
}

actualizar mago = map (cambiarMago mago)
cambiarMago nuevoMago horro =horro{mago = nuevoMago}

destruir :: Horrocrux -> Mago
destruir horro = quitar horro (mago horro)

quitar horro mago = nuevoMago
    where nuevoMago = mago{ horrocruxes = actualizar nuevoMago (filter ((/= denominacion horro).denominacion) (horrocruxes mago))}

finalFeliz [] = False
finalFeliz  (horro:horrocruxes) 
    | sigueConVida (destruir horro) = finalFeliz (actualizar (destruir horro) horrocruxes)
    | otherwise = True



sigueConVida = not.null.horrocruxes





correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de Voldemort" $ do
    it "final feliz" $ do
      finalFeliz [harry,diadema] `shouldBe` True
    it "final desafortunado" $ do
      finalFeliz [harry] `shouldBe` False
    it "otro final feliz" $ do
      finalFeliz [harry,diadema] `shouldBe` True
-}