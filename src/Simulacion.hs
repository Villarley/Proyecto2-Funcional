module Simulacion
  ( simularReduccionGastos
  , proyeccionAhorro
  ) where

import Types

totalGastos :: [Registro] -> Double
totalGastos = foldl' (\acc r -> if tipoRegistro r == Gasto then acc + monto r else acc) 0.0

totalIngresos :: [Registro] -> Double
totalIngresos = foldl' (\acc r -> if tipoRegistro r == Ingreso then acc + monto r else acc) 0.0

simularReduccionGastos :: Double -> [Registro] -> (Double, Double)
simularReduccionGastos porcentaje rs =
  let gastoActual  = totalGastos rs
      gastoNuevo   = gastoActual * (1.0 - porcentaje / 100.0)
      ahorro       = gastoActual - gastoNuevo
  in (gastoNuevo, ahorro)

proyeccionAhorro :: Int -> [Registro] -> [(Int, Double)]
proyeccionAhorro meses rs =
  let ingresos    = totalIngresos rs
      gastos      = totalGastos rs
      ahorroMes   = ingresos - gastos
  in [ (m, ahorroMes * fromIntegral m) | m <- [1..meses] ]
