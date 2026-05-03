module Reportes
  ( resumenMensual
  , comparacionPeriodos
  , categoriasMayorGasto
  ) where

import Types
import Data.List (nub, sortBy)
import Data.Ord (comparing, Down(..))
import Data.Time (toGregorian)

mes :: Registro -> (Integer, Int)
mes r = let (y, m, _) = toGregorian (fecha r) in (y, m)

totalTipo :: TipoRegistro -> [Registro] -> Double
totalTipo t = foldl' (\acc r -> if tipoRegistro r == t then acc + monto r else acc) 0.0

resumenMensual :: Integer -> Int -> [Registro] -> String
resumenMensual year month rs =
  let del      = filter (\r -> mes r == (year, month)) rs
      ing      = totalTipo Ingreso del
      gas      = totalTipo Gasto del
      aho      = totalTipo Ahorro del
      inv      = totalTipo Inversion del
      flujo    = ing - gas
  in unlines
       [ "=== Resumen " ++ show year ++ "-" ++ show month ++ " ==="
       , "Ingresos:    " ++ show ing
       , "Gastos:      " ++ show gas
       , "Ahorros:     " ++ show aho
       , "Inversiones: " ++ show inv
       , "Flujo neto:  " ++ show flujo
       ]

comparacionPeriodos :: (Integer, Int) -> (Integer, Int) -> [Registro] -> String
comparacionPeriodos p1 p2 rs =
  let del1  = filter (\r -> mes r == p1) rs
      del2  = filter (\r -> mes r == p2) rs
      gas1  = totalTipo Gasto del1
      gas2  = totalTipo Gasto del2
      diff  = gas2 - gas1
      label = if diff >= 0 then "aumento" else "disminución"
  in unlines
       [ "=== Comparación de períodos ==="
       , show p1 ++ " gastos: " ++ show gas1
       , show p2 ++ " gastos: " ++ show gas2
       , "Diferencia (" ++ label ++ "): " ++ show (abs diff)
       ]

categoriasMayorGasto :: [Registro] -> [(Categoria, Double)]
categoriasMayorGasto rs =
  let gastos = filter (\r -> tipoRegistro r == Gasto) rs
      cats   = nub (map categoria gastos)
      totales = [ (c, foldl' (\a r -> if categoria r == c then a + monto r else a) 0.0 gastos)
                | c <- cats ]
  in sortBy (comparing (Down . snd)) totales
