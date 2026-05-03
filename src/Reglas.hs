module Reglas
  ( agregarRegla
  , evaluarReglas
  ) where

import Types

agregarRegla :: Regla -> [Regla] -> [Regla]
agregarRegla r rs = r : rs

totalPorCategoria :: TipoRegistro -> Categoria -> [Registro] -> Double
totalPorCategoria tipo cat rs =
  foldl' (\acc r -> if tipoRegistro r == tipo && categoria r == cat
                    then acc + monto r
                    else acc) 0.0 rs

evaluarReglas :: [Regla] -> [Registro] -> [Alerta]
evaluarReglas reglas rs = concatMap evaluar reglas
  where
    evaluar regla =
      let tipoReg = case reglaTipo regla of
                      AlertaPresupuesto  -> Gasto
                      AdvertenciaAhorro -> Ahorro
          total = totalPorCategoria tipoReg (reglaCategoria regla) rs
          condicion = case reglaTipo regla of
                        AlertaPresupuesto  -> total > reglaLimite regla
                        AdvertenciaAhorro -> total < reglaLimite regla
      in if condicion
           then [ Alerta (reglaTipo regla)
                    ("Regla activada para " ++ show (reglaCategoria regla) ++
                     ": valor " ++ show total ++
                     " / umbral " ++ show (reglaLimite regla)) ]
           else []
