module Storage
  ( cargarRegistros
  , guardarRegistros
  , cargarPresupuestos
  , guardarPresupuestos
  , cargarReglas
  , guardarReglas
  ) where

import Types
import System.Directory (doesFileExist)

archivoDatos :: FilePath
archivoDatos = "datos.txt"

archivoPresupuestos :: FilePath
archivoPresupuestos = "presupuestos.txt"

archivoReglas :: FilePath
archivoReglas = "reglas.txt"

cargarRegistros :: IO [Registro]
cargarRegistros = cargarArchivo archivoDatos

guardarRegistros :: [Registro] -> IO ()
guardarRegistros = guardarArchivo archivoDatos

cargarPresupuestos :: IO [Presupuesto]
cargarPresupuestos = cargarArchivo archivoPresupuestos

guardarPresupuestos :: [Presupuesto] -> IO ()
guardarPresupuestos = guardarArchivo archivoPresupuestos

cargarReglas :: IO [Regla]
cargarReglas = cargarArchivo archivoReglas

guardarReglas :: [Regla] -> IO ()
guardarReglas = guardarArchivo archivoReglas

cargarArchivo :: (Read a) => FilePath -> IO [a]
cargarArchivo path = do
  existe <- doesFileExist path
  if existe
    then do
      contenido <- readFile path
      let lineas = filter (not . null) (lines contenido)
      return (map read lineas)
    else return []

guardarArchivo :: (Show a) => FilePath -> [a] -> IO ()
guardarArchivo path items =
  writeFile path (unlines (map show items))
