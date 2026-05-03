module Main (main) where

import Types
import Storage
import Registros
import Presupuestos
import Analisis
import Simulacion
import Reglas
import Reportes

import Data.Time (fromGregorian)
import Data.IORef

main :: IO ()
main = do
  registros    <- cargarRegistros
  presupuestos <- cargarPresupuestos
  reglas       <- cargarReglas
  refReg  <- newIORef registros
  refPres <- newIORef presupuestos
  refReg2 <- newIORef reglas
  putStrLn "=== Sistema de Finanzas Personales (Haskell) ==="
  loop refReg refPres refReg2

loop :: IORef [Registro] -> IORef [Presupuesto] -> IORef [Regla] -> IO ()
loop refReg refPres refReglas = do
  putStrLn ""
  putStrLn "1. Agregar registro"
  putStrLn "2. Listar registros"
  putStrLn "3. Ver análisis financiero"
  putStrLn "4. Presupuestos"
  putStrLn "5. Simulación"
  putStrLn "6. Reportes"
  putStrLn "7. Alertas y reglas"
  putStrLn "0. Salir"
  putStr "Opción: "
  opcion <- getLine
  case opcion of
    "0" -> do
      rs <- readIORef refReg
      guardarRegistros rs
      ps <- readIORef refPres
      guardarPresupuestos ps
      rgs <- readIORef refReglas
      guardarReglas rgs
      putStrLn "Datos guardados. ¡Hasta luego!"
    "1" -> menuAgregarRegistro refReg >> loop refReg refPres refReglas
    "2" -> menuListar refReg          >> loop refReg refPres refReglas
    "3" -> menuAnalisis refReg        >> loop refReg refPres refReglas
    "4" -> menuPresupuestos refPres refReg >> loop refReg refPres refReglas
    "5" -> menuSimulacion refReg      >> loop refReg refPres refReglas
    "6" -> menuReportes refReg        >> loop refReg refPres refReglas
    "7" -> menuReglas refReglas refReg >> loop refReg refPres refReglas
    _   -> putStrLn "Opción inválida." >> loop refReg refPres refReglas

menuAgregarRegistro :: IORef [Registro] -> IO ()
menuAgregarRegistro refReg = do
  rs <- readIORef refReg
  putStr "Tipo (Ingreso/Gasto/Ahorro/Inversion): "
  tipoStr <- getLine
  let tipo = read tipoStr :: TipoRegistro
  putStr "Monto: "
  montoStr <- getLine
  let mont = read montoStr :: Double
  putStr "Categoría (Alimentacion/Transporte/Vivienda/Entretenimiento/Salud/Educacion): "
  catStr <- getLine
  let cat = read catStr :: Categoria
  putStr "Fecha (YYYY-MM-DD): "
  fechaStr <- getLine
  let parts = splitOn '-' fechaStr
      y   = read (parts !! 0) :: Integer
      m   = read (parts !! 1) :: Int
      d   = read (parts !! 2) :: Int
      dia = fromGregorian y m d
  putStr "Descripción: "
  desc <- getLine
  putStr "Etiquetas (separadas por coma): "
  etStr <- getLine
  let ets = splitOn ',' etStr
  let nuevo = Registro (siguienteId rs) tipo mont cat dia desc ets
  writeIORef refReg (agregarRegistro nuevo rs)
  putStrLn "Registro agregado."

menuListar :: IORef [Registro] -> IO ()
menuListar refReg = do
  rs <- readIORef refReg
  if null rs
    then putStrLn "No hay registros."
    else mapM_ print rs

menuAnalisis :: IORef [Registro] -> IO ()
menuAnalisis refReg = do
  rs <- readIORef refReg
  putStrLn "\n-- Flujo de caja mensual --"
  mapM_ (\(m, f) -> putStrLn (m ++ ": " ++ show f)) (flujoCajaMensual rs)
  putStrLn "\n-- Proyección promedio mensual de gastos --"
  print (proyeccionGastos rs)
  putStrLn "\n-- Categoría con mayor impacto --"
  print (categoriaMayorImpacto rs)

menuPresupuestos :: IORef [Presupuesto] -> IORef [Registro] -> IO ()
menuPresupuestos refPres refReg = do
  putStrLn "1. Agregar presupuesto  2. Ver alertas"
  op <- getLine
  case op of
    "1" -> do
      putStr "Categoría: "
      catStr <- getLine
      let cat = read catStr :: Categoria
      putStr "Límite: "
      limStr <- getLine
      let lim = read limStr :: Double
      modifyIORef refPres (agregarPresupuesto (Presupuesto cat lim))
      putStrLn "Presupuesto guardado."
    "2" -> do
      ps <- readIORef refPres
      rs <- readIORef refReg
      let alertas = verificarPresupuestos ps rs
      if null alertas then putStrLn "Sin alertas."
      else mapM_ (putStrLn . mensaje) alertas
    _ -> putStrLn "Opción inválida."

menuSimulacion :: IORef [Registro] -> IO ()
menuSimulacion refReg = do
  rs <- readIORef refReg
  putStr "Porcentaje de reducción de gastos (%): "
  pStr <- getLine
  let p = read pStr :: Double
  let (nuevo, ahorro) = simularReduccionGastos p rs
  putStrLn ("Gastos actuales → reducidos a: " ++ show nuevo)
  putStrLn ("Ahorro mensual estimado: " ++ show ahorro)
  putStr "Proyección de ahorro acumulado a N meses: "
  nStr <- getLine
  let n = read nStr :: Int
  mapM_ (\(m, a) -> putStrLn ("Mes " ++ show m ++ ": " ++ show a))
        (proyeccionAhorro n rs)

menuReportes :: IORef [Registro] -> IO ()
menuReportes refReg = do
  rs <- readIORef refReg
  putStr "Año (ej. 2026): "
  y <- fmap read getLine
  putStr "Mes (1-12): "
  m <- fmap read getLine
  putStrLn (resumenMensual y m rs)
  putStrLn "-- Categorías con mayor gasto --"
  mapM_ print (categoriasMayorGasto rs)

menuReglas :: IORef [Regla] -> IORef [Registro] -> IO ()
menuReglas refReglas refReg = do
  putStrLn "1. Agregar regla  2. Evaluar reglas"
  op <- getLine
  case op of
    "1" -> do
      putStr "Categoría: "
      catStr <- getLine
      let cat = read catStr :: Categoria
      putStr "Límite: "
      limStr <- getLine
      let lim = read limStr :: Double
      putStr "Tipo (AlertaPresupuesto/AdvertenciaAhorro): "
      tStr <- getLine
      let t = read tStr :: TipoAlerta
      modifyIORef refReglas (agregarRegla (Regla cat lim t))
      putStrLn "Regla guardada."
    "2" -> do
      rgs <- readIORef refReglas
      rs  <- readIORef refReg
      let alertas = evaluarReglas rgs rs
      if null alertas then putStrLn "Ninguna regla activada."
      else mapM_ (putStrLn . mensaje) alertas
    _ -> putStrLn "Opción inválida."

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn sep (c:cs)
  | c == sep  = "" : splitOn sep cs
  | otherwise = case splitOn sep cs of
                  []     -> [[c]]
                  (w:ws) -> (c:w) : ws
