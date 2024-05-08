{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
data Color = Rojo | Negro | Azul | Verde

-- Tarea: fila de Gobstones
data Fila
  = Final
  | Celda (Color -> Int) Fila

ejemploFila :: Fila
ejemploFila =
  Celda
    ( \color -> case color of
        Rojo -> 2
        Negro -> 1
        Azul -> 0
        Verde -> 3
    )
    Final

-- Asistencia:
-- 1) Dar las reglas que definen al conjunto
-- inductivo Fila (basado el tipo algebraico Fila definido anteriormente)
-- 2) dar forma esquemática de una función
-- definida por recursión estructural sobre Fila
-- 3)
nroBolitas :: Color -> Fila -> Int
nroBolitas _ Final = 0
nroBolitas color (Celda f fila) = f color + nroBolitas color fila

hayBolitas :: Color -> Fila -> Bool
hayBolitas _ Final = False
hayBolitas color (Celda f fila) = f color > 0 || hayBolitas color fila

poner :: Color -> Int -> Fila -> Fila
poner _ _ Final = Final -- Caso base: no hay más celdas para poner bolitas
poner color cantidad (Celda f fila) =
  case color of
    Rojo -> Celda (\c -> if esMismoColor c Rojo then cantidad + f Rojo else f Rojo) (poner color cantidad fila)
    Negro -> Celda (\c -> if esMismoColor c Negro then cantidad + f Negro else f Negro) (poner color cantidad fila)
    Azul -> Celda (\c -> if esMismoColor c Azul then cantidad + f Azul else f Azul) (poner color cantidad fila)
    Verde -> Celda (\c -> if esMismoColor c Verde then cantidad + f Verde else f Verde) (poner color cantidad fila)

esMismoColor :: Color -> Color -> Bool
esMismoColor Rojo Rojo = True
esMismoColor Azul Azul = True
esMismoColor Verde Verde = True
esMismoColor Negro Negro = True
esMismoColor _ _ = False
