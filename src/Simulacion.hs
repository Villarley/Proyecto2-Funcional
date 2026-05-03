module Simulacion
  ( simularReduccionGastos
  , proyeccionAhorro
  ) where

import Types
import Analisis (promedioMensual)

-- Simula reducir el gasto mensual promedio en un porcentaje.
-- Retorna (nuevo gasto mensual, ahorro extra mensual).
simularReduccionGastos :: Double -> [Registro] -> (Double, Double)
simularReduccionGastos porcentaje rs =
  let gastoMes   = promedioMensual Gasto rs
      gastoNuevo = gastoMes * (1.0 - porcentaje / 100.0)
      ahorroExtra = gastoMes - gastoNuevo
  in (gastoNuevo, ahorroExtra)

-- Proyecta el ahorro acumulado a N meses usando el superávit mensual promedio.
proyeccionAhorro :: Int -> [Registro] -> [(Int, Double)]
proyeccionAhorro n rs =
  let ingresosMes = promedioMensual Ingreso rs
      gastosMes   = promedioMensual Gasto   rs
      superavit   = ingresosMes - gastosMes
  in [ (m, superavit * fromIntegral m) | m <- [1..n] ]
