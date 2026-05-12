module Simulacion
  ( simularReduccionGastos
  , proyeccionAhorroAcumulado
  ) where

import Types
import Analisis (promedioMensual)

-- Simula reducir el gasto mensual promedio en un porcentaje
simularReduccionGastos :: Double -> [Registro] -> (Double, Double)
simularReduccionGastos porcentaje rs =
  let gastoMes   = promedioMensual Gasto rs
      gastoNuevo = gastoMes * (1.0 - porcentaje / 100.0)
      ahorroExtra = gastoMes - gastoNuevo
  in (gastoNuevo, ahorroExtra)

-- Ahorro acumulado si cada mes ahorra el mismo monto
proyeccionAhorroAcumulado :: Double -> Int -> [(Int, Double)]
proyeccionAhorroAcumulado porMes n =
  [ (m, porMes * fromIntegral m) | m <- [1..n] ]
