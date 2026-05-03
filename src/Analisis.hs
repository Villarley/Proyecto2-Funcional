module Analisis
  ( flujoCajaMensual
  , tendenciaGasto
  , proyeccionGastos
  , categoriaMayorImpacto
  ) where

import Types
import Data.List (maximumBy, nub, sortBy)
import Data.Ord (comparing)
import Data.Time (toGregorian)

mes :: Registro -> (Integer, Int)
mes r = let (y, m, _) = toGregorian (fecha r) in (y, m)

totalTipo :: TipoRegistro -> [Registro] -> Double
totalTipo t = foldl' (\acc r -> if tipoRegistro r == t then acc + monto r else acc) 0.0

flujoCajaMensual :: [Registro] -> [(String, Double)]
flujoCajaMensual rs =
  let meses = nub (map mes rs)
      flujo (y, m) =
        let del = filter (\r -> mes r == (y, m)) rs
            ing = totalTipo Ingreso del
            gas = totalTipo Gasto   del
        in (show y ++ "-" ++ show m, ing - gas)
  in map flujo (sortBy (comparing id) meses)

tendenciaGasto :: [Registro] -> [(String, Double)]
tendenciaGasto rs =
  let gastos = filter (\r -> tipoRegistro r == Gasto) rs
      meses  = nub (map mes gastos)
      totalMes (y, m) = totalTipo Gasto (filter (\r -> mes r == (y, m)) gastos)
  in [ (show y ++ "-" ++ show m, totalMes (y, m))
     | (y, m) <- sortBy (comparing id) meses ]

proyeccionGastos :: [Registro] -> Double
proyeccionGastos rs =
  let totales = map snd (tendenciaGasto rs)
  in if null totales then 0.0
     else sum totales / fromIntegral (length totales)

categoriaMayorImpacto :: [Registro] -> Maybe Categoria
categoriaMayorImpacto rs =
  let gastos = filter (\r -> tipoRegistro r == Gasto) rs
      cats   = nub (map categoria gastos)
      totales = [ (c, foldl' (\a r -> if categoria r == c then a + monto r else a) 0.0 gastos)
                | c <- cats ]
  in if null totales then Nothing
     else Just (fst (maximumBy (comparing snd) totales))
