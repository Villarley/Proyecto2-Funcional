module Presupuestos
  ( agregarPresupuesto
  , gastoRealPorCategoria
  , verificarPresupuestos
  ) where

import Types

agregarPresupuesto :: Presupuesto -> [Presupuesto] -> [Presupuesto]
agregarPresupuesto p ps =
  p : filter (\x -> presCategoria x /= presCategoria p) ps

gastoRealPorCategoria :: Categoria -> [Registro] -> Double
gastoRealPorCategoria cat rs =
  foldl' (\acc r -> if categoria r == cat && tipoRegistro r == Gasto
                    then acc + monto r
                    else acc) 0.0 rs

verificarPresupuestos :: [Presupuesto] -> [Registro] -> [Alerta]
verificarPresupuestos ps rs =
  [ Alerta AlertaPresupuesto
      ("Presupuesto excedido en " ++ show (presCategoria p) ++
       ": gastado " ++ show gastado ++
       " / límite " ++ show (presLimite p))
  | p <- ps
  , let gastado = gastoRealPorCategoria (presCategoria p) rs
  , gastado > presLimite p
  ]
