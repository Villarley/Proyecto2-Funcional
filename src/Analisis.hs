module Analisis
  ( flujoCajaMensual
  , tendenciaGasto
  , proyeccionGastos
  , categoriaMayorImpacto
  , gastosPorCategoria
  , promedioMensual
  ) where

import Types
import Data.List (maximumBy, nub, sortBy)
import Data.Ord (comparing)
import Data.Time (toGregorian)

mesReg :: Registro -> (Integer, Int)
mesReg r = let (y, m, _) = toGregorian (fecha r) in (y, m)

labelMes :: (Integer, Int) -> String
labelMes (y, m) = show y ++ "-" ++ (if m < 10 then "0" else "") ++ show m

totalTipoEn :: TipoRegistro -> [Registro] -> Double
totalTipoEn t rs = foldl' (\acc r -> if tipoRegistro r == t then acc + monto r else acc) 0.0 rs

-- Promedio de monto mensual para un tipo de registro dado.
promedioMensual :: TipoRegistro -> [Registro] -> Double
promedioMensual tipo rs =
  let del    = filter (\r -> tipoRegistro r == tipo) rs
      meses  = nub (map mesReg del)
      nMeses = max 1 (length meses)
      total  = foldl' (\acc r -> acc + monto r) 0.0 del
  in total / fromIntegral nMeses

flujoCajaMensual :: [Registro] -> [(String, Double)]
flujoCajaMensual rs =
  let meses = sortBy (comparing id) (nub (map mesReg rs))
      flujo ym =
        let del = filter (\r -> mesReg r == ym) rs
            ing = totalTipoEn Ingreso del
            gas = totalTipoEn Gasto   del
        in (labelMes ym, ing - gas)
  in map flujo meses

tendenciaGasto :: [Registro] -> [(String, Double)]
tendenciaGasto rs =
  let gastos = filter (\r -> tipoRegistro r == Gasto) rs
      meses  = sortBy (comparing id) (nub (map mesReg gastos))
  in [ (labelMes ym, totalTipoEn Gasto (filter (\r -> mesReg r == ym) gastos))
     | ym <- meses ]

-- Proyección: promedio mensual de gastos históricos.
proyeccionGastos :: [Registro] -> Double
proyeccionGastos = promedioMensual Gasto

-- Categoría que acumula más gastos en total.
categoriaMayorImpacto :: [Registro] -> Maybe Categoria
categoriaMayorImpacto rs =
  let breakdown = gastosPorCategoria rs
  in if null breakdown then Nothing
     else Just (fst (maximumBy (comparing snd) breakdown))

-- Desglose de gastos totales agrupados por categoría.
gastosPorCategoria :: [Registro] -> [(Categoria, Double)]
gastosPorCategoria rs =
  let gastos = filter (\r -> tipoRegistro r == Gasto) rs
      cats   = nub (map categoria gastos)
  in sortBy (comparing snd)
       [ (c, foldl' (\a r -> if categoria r == c then a + monto r else a) 0.0 gastos)
       | c <- cats ]
