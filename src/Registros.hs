module Registros
  ( agregarRegistro
  , eliminarRegistro
  , listarRegistros
  , filtrarPorTipo
  , filtrarPorCategoria
  , filtrarPorFecha
  , siguienteId
  ) where

import Types
import Data.Time (Day)

siguienteId :: [Registro] -> Int
siguienteId [] = 1
siguienteId rs = maximum (map registroId rs) + 1

agregarRegistro :: Registro -> [Registro] -> [Registro]
agregarRegistro r rs = rs ++ [r]

eliminarRegistro :: Int -> [Registro] -> [Registro]
eliminarRegistro rid = filter (\r -> registroId r /= rid)

listarRegistros :: [Registro] -> [Registro]
listarRegistros = id

filtrarPorTipo :: TipoRegistro -> [Registro] -> [Registro]
filtrarPorTipo t = filter (\r -> tipoRegistro r == t)

filtrarPorCategoria :: Categoria -> [Registro] -> [Registro]
filtrarPorCategoria c = filter (\r -> categoria r == c)

filtrarPorFecha :: Day -> Day -> [Registro] -> [Registro]
filtrarPorFecha desde hasta = filter (\r -> fecha r >= desde && fecha r <= hasta)
