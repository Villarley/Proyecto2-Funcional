module Main (main) where

import Types
import Storage
import Registros
import Presupuestos
import Analisis
import Simulacion
import Reglas
import Reportes

import Data.List (intercalate)
import Data.Time (Day, fromGregorian)
import Data.IORef

-- ─── Entry point ──────────────────────────────────────────────────────────────

main :: IO ()
main = do
  registros    <- cargarRegistros
  presupuestos <- cargarPresupuestos
  reglas       <- cargarReglas
  refReg   <- newIORef registros
  refPres  <- newIORef presupuestos
  refRegl  <- newIORef reglas
  putStrLn "╔══════════════════════════════════════════╗"
  putStrLn "║   Sistema de Finanzas Personales (Haskell)  ║"
  putStrLn "╚══════════════════════════════════════════╝"
  menuPrincipal refReg refPres refRegl

-- ─── Menú principal ───────────────────────────────────────────────────────────

menuPrincipal :: IORef [Registro] -> IORef [Presupuesto] -> IORef [Regla] -> IO ()
menuPrincipal refReg refPres refRegl = do
  putStrLn ""
  putStrLn "──── MENÚ PRINCIPAL ────"
  putStrLn "  1. Registros financieros"
  putStrLn "  2. Presupuestos"
  putStrLn "  3. Reglas y alertas"
  putStrLn "  4. Análisis financiero"
  putStrLn "  5. Simulación"
  putStrLn "  6. Reportes"
  putStrLn "  0. Guardar y salir"
  putStr "Opción: "
  op <- getLine
  case op of
    "0" -> guardarTodo refReg refPres refRegl
    "1" -> menuRegistros refReg         >> menuPrincipal refReg refPres refRegl
    "2" -> menuPresupuestos refPres refReg >> menuPrincipal refReg refPres refRegl
    "3" -> menuReglas refRegl refReg    >> menuPrincipal refReg refPres refRegl
    "4" -> menuAnalisis refReg          >> menuPrincipal refReg refPres refRegl
    "5" -> menuSimulacion refReg        >> menuPrincipal refReg refPres refRegl
    "6" -> menuReportes refReg          >> menuPrincipal refReg refPres refRegl
    _   -> putStrLn "Opción inválida."  >> menuPrincipal refReg refPres refRegl

guardarTodo :: IORef [Registro] -> IORef [Presupuesto] -> IORef [Regla] -> IO ()
guardarTodo refReg refPres refRegl = do
  readIORef refReg  >>= guardarRegistros
  readIORef refPres >>= guardarPresupuestos
  readIORef refRegl >>= guardarReglas
  putStrLn "Datos guardados. ¡Hasta luego!"

-- ─── Registros ────────────────────────────────────────────────────────────────

menuRegistros :: IORef [Registro] -> IO ()
menuRegistros refReg = do
  putStrLn ""
  putStrLn "── REGISTROS ──"
  putStrLn "  1. Agregar registro"
  putStrLn "  2. Listar todos"
  putStrLn "  3. Filtrar registros"
  putStrLn "  4. Eliminar registro"
  putStrLn "  0. Volver"
  putStr "Opción: "
  op <- getLine
  case op of
    "1" -> agregarRegistroMenu refReg >> menuRegistros refReg
    "2" -> mostrarTodosRegistros refReg >> menuRegistros refReg
    "3" -> filtrarMenu refReg         >> menuRegistros refReg
    "4" -> eliminarRegistroMenu refReg >> menuRegistros refReg
    "0" -> return ()
    _   -> putStrLn "Opción inválida." >> menuRegistros refReg

agregarRegistroMenu :: IORef [Registro] -> IO ()
agregarRegistroMenu refReg = do
  rs   <- readIORef refReg
  tipo <- pedirTipo
  putStr "Monto: "
  mont <- fmap read getLine :: IO Double
  cat  <- pedirCategoria
  putStr "Fecha (YYYY-MM-DD): "
  dia  <- pedirFecha
  putStr "Descripción: "
  desc <- getLine
  putStr "Etiquetas (separadas por coma, o Enter para ninguna): "
  etStr <- getLine
  let ets = if null etStr then [] else splitOn ',' etStr
  let nuevo = Registro (siguienteId rs) tipo mont cat dia desc ets
  writeIORef refReg (agregarRegistro nuevo rs)
  putStrLn (" Registro #" ++ show (registroId nuevo) ++ " agregado.")

mostrarTodosRegistros :: IORef [Registro] -> IO ()
mostrarTodosRegistros refReg = do
  rs <- readIORef refReg
  if null rs
    then putStrLn "No hay registros."
    else mapM_ (putStrLn . mostrarRegistro) rs

filtrarMenu :: IORef [Registro] -> IO ()
filtrarMenu refReg = do
  putStrLn "  Filtrar por:  1. Tipo  2. Categoría  3. Fecha"
  putStr "Opción: "
  op <- getLine
  rs <- readIORef refReg
  case op of
    "1" -> do
      tipo <- pedirTipo
      let filtrados = filtrarPorTipo tipo rs
      if null filtrados then putStrLn "Sin resultados."
      else mapM_ (putStrLn . mostrarRegistro) filtrados
    "2" -> do
      cat <- pedirCategoria
      let filtrados = filtrarPorCategoria cat rs
      if null filtrados then putStrLn "Sin resultados."
      else mapM_ (putStrLn . mostrarRegistro) filtrados
    "3" -> do
      putStr "Fecha inicio (YYYY-MM-DD): "
      desde <- pedirFecha
      putStr "Fecha fin    (YYYY-MM-DD): "
      hasta <- pedirFecha
      let filtrados = filtrarPorFecha desde hasta rs
      if null filtrados then putStrLn "Sin resultados."
      else mapM_ (putStrLn . mostrarRegistro) filtrados
    _ -> putStrLn "Opción inválida."

eliminarRegistroMenu :: IORef [Registro] -> IO ()
eliminarRegistroMenu refReg = do
  rs <- readIORef refReg
  if null rs
    then putStrLn "No hay registros."
    else do
      mapM_ (\r -> putStrLn ("[" ++ show (registroId r) ++ "] " ++
                             show (tipoRegistro r) ++ " | " ++
                             show (monto r) ++ " | " ++
                             descripcion r)) rs
      putStr "ID a eliminar: "
      rid <- fmap read getLine :: IO Int
      writeIORef refReg (eliminarRegistro rid rs)
      putStrLn " Registro eliminado."

-- ─── Presupuestos ─────────────────────────────────────────────────────────────

menuPresupuestos :: IORef [Presupuesto] -> IORef [Registro] -> IO ()
menuPresupuestos refPres refReg = do
  putStrLn ""
  putStrLn "── PRESUPUESTOS ──"
  putStrLn "  1. Agregar presupuesto"
  putStrLn "  2. Ver presupuestos actuales"
  putStrLn "  3. Ver alertas"
  putStrLn "  4. Eliminar presupuesto"
  putStrLn "  0. Volver"
  putStr "Opción: "
  op <- getLine
  case op of
    "1" -> agregarPresupuestoMenu refPres >> menuPresupuestos refPres refReg
    "2" -> listarPresupuestos refPres refReg >> menuPresupuestos refPres refReg
    "3" -> verAlertasPresupuesto refPres refReg >> menuPresupuestos refPres refReg
    "4" -> eliminarPresupuestoMenu refPres >> menuPresupuestos refPres refReg
    "0" -> return ()
    _   -> putStrLn "Opción inválida." >> menuPresupuestos refPres refReg

agregarPresupuestoMenu :: IORef [Presupuesto] -> IO ()
agregarPresupuestoMenu refPres = do
  cat <- pedirCategoria
  putStr "Límite mensual: "
  lim <- fmap read getLine :: IO Double
  modifyIORef refPres (agregarPresupuesto (Presupuesto cat lim))
  putStrLn " Presupuesto guardado."

listarPresupuestos :: IORef [Presupuesto] -> IORef [Registro] -> IO ()
listarPresupuestos refPres refReg = do
  ps <- readIORef refPres
  rs <- readIORef refReg
  if null ps
    then putStrLn "No hay presupuestos definidos."
    else mapM_ (putStrLn . mostrarPresupuesto rs) ps

verAlertasPresupuesto :: IORef [Presupuesto] -> IORef [Registro] -> IO ()
verAlertasPresupuesto refPres refReg = do
  ps <- readIORef refPres
  rs <- readIORef refReg
  let alertas = verificarPresupuestos ps rs
  if null alertas
    then putStrLn " Ningún presupuesto excedido."
    else mapM_ (putStrLn . (" " ++) . mensaje) alertas

eliminarPresupuestoMenu :: IORef [Presupuesto] -> IO ()
eliminarPresupuestoMenu refPres = do
  ps <- readIORef refPres
  if null ps
    then putStrLn "No hay presupuestos."
    else do
      mapM_ (\(i, p) -> putStrLn ("[" ++ show i ++ "] " ++ mostrarCategoria (presCategoria p) ++
                                  " - límite: " ++ show (presLimite p)))
            (zip ([1..] :: [Int]) ps)
      putStr "Número a eliminar: "
      idx <- fmap read getLine :: IO Int
      let nuevos = [ p | (i, p) <- zip ([1..] :: [Int]) ps, i /= idx ]
      writeIORef refPres nuevos
      putStrLn " Presupuesto eliminado."

-- ─── Reglas ───────────────────────────────────────────────────────────────────

menuReglas :: IORef [Regla] -> IORef [Registro] -> IO ()
menuReglas refRegl refReg = do
  putStrLn ""
  putStrLn "── REGLAS Y ALERTAS ──"
  putStrLn "  1. Agregar regla"
  putStrLn "  2. Ver reglas"
  putStrLn "  3. Evaluar reglas"
  putStrLn "  4. Eliminar regla"
  putStrLn "  0. Volver"
  putStr "Opción: "
  op <- getLine
  case op of
    "1" -> agregarReglaMenu refRegl  >> menuReglas refRegl refReg
    "2" -> listarReglas refRegl      >> menuReglas refRegl refReg
    "3" -> evaluarReglasMenu refRegl refReg >> menuReglas refRegl refReg
    "4" -> eliminarReglaMenu refRegl >> menuReglas refRegl refReg
    "0" -> return ()
    _   -> putStrLn "Opción inválida." >> menuReglas refRegl refReg

agregarReglaMenu :: IORef [Regla] -> IO ()
agregarReglaMenu refRegl = do
  cat <- pedirCategoria
  putStr "Umbral (monto): "
  lim <- fmap read getLine :: IO Double
  putStrLn "Tipo de alerta:"
  putStrLn "  1. Alerta de presupuesto (si gasto > umbral)"
  putStrLn "  2. Advertencia de ahorro (si ahorro < umbral)"
  putStr "Opción: "
  tOp <- getLine
  let t = case tOp of
            "2" -> AdvertenciaAhorro
            _   -> AlertaPresupuesto
  modifyIORef refRegl (agregarRegla (Regla cat lim t))
  putStrLn " Regla guardada."

listarReglas :: IORef [Regla] -> IO ()
listarReglas refRegl = do
  rs <- readIORef refRegl
  if null rs
    then putStrLn "No hay reglas definidas."
    else mapM_ (\(i, r) -> putStrLn ("[" ++ show i ++ "] " ++ mostrarRegla r))
               (zip ([1..] :: [Int]) rs)

evaluarReglasMenu :: IORef [Regla] -> IORef [Registro] -> IO ()
evaluarReglasMenu refRegl refReg = do
  regl <- readIORef refRegl
  rs   <- readIORef refReg
  let alertas = evaluarReglas regl rs
  if null alertas
    then putStrLn " Ninguna regla activada."
    else mapM_ (putStrLn . (" " ++) . mensaje) alertas

eliminarReglaMenu :: IORef [Regla] -> IO ()
eliminarReglaMenu refRegl = do
  rs <- readIORef refRegl
  if null rs
    then putStrLn "No hay reglas."
    else do
      mapM_ (\(i, r) -> putStrLn ("[" ++ show i ++ "] " ++ mostrarRegla r))
            (zip ([1..] :: [Int]) rs)
      putStr "Número a eliminar: "
      idx <- fmap read getLine :: IO Int
      writeIORef refRegl [ r | (i, r) <- zip ([1..] :: [Int]) rs, i /= idx ]
      putStrLn " Regla eliminada."

-- ─── Análisis ─────────────────────────────────────────────────────────────────

menuAnalisis :: IORef [Registro] -> IO ()
menuAnalisis refReg = do
  rs <- readIORef refReg
  putStrLn ""
  putStrLn "── ANÁLISIS FINANCIERO ──"
  putStrLn "\n[Flujo de caja mensual]"
  let flujo = flujoCajaMensual rs
  if null flujo then putStrLn "Sin datos."
  else mapM_ (\(m, f) -> putStrLn ("  " ++ m ++ ": " ++ formatMonto f)) flujo
  putStrLn "\n[Tendencia de gastos]"
  let tend = tendenciaGasto rs
  if null tend then putStrLn "Sin datos."
  else mapM_ (\(m, t) -> putStrLn ("  " ++ m ++ ": " ++ formatMonto t)) tend
  putStrLn "\n[Proyección promedio mensual de gastos]"
  putStrLn ("  " ++ formatMonto (proyeccionGastos rs))
  putStrLn "\n[Categoría con mayor impacto financiero]"
  case categoriaMayorImpacto rs of
    Nothing  -> putStrLn "  Sin datos."
    Just cat -> putStrLn ("  " ++ mostrarCategoria cat)

-- ─── Simulación ───────────────────────────────────────────────────────────────

menuSimulacion :: IORef [Registro] -> IO ()
menuSimulacion refReg = do
  rs <- readIORef refReg
  putStrLn ""
  putStrLn "── SIMULACIÓN FINANCIERA ──"
  putStr "Porcentaje de reducción de gastos (%): "
  p <- fmap read getLine :: IO Double
  let (gastoNuevo, ahorro) = simularReduccionGastos p rs
  putStrLn ("\n  Gastos actuales reducidos a: " ++ formatMonto gastoNuevo)
  putStrLn ("  Ahorro mensual estimado:     " ++ formatMonto ahorro)
  putStr "\nProyectar ahorro acumulado a cuántos meses: "
  n <- fmap read getLine :: IO Int
  putStrLn ""
  mapM_ (\(mes, ac) -> putStrLn ("  Mes " ++ show mes ++ ": " ++ formatMonto ac))
        (proyeccionAhorro n rs)

-- ─── Reportes ─────────────────────────────────────────────────────────────────

menuReportes :: IORef [Registro] -> IO ()
menuReportes refReg = do
  rs <- readIORef refReg
  putStrLn ""
  putStrLn "── REPORTES ──"
  putStr "Año (ej. 2026): "
  y <- fmap read getLine :: IO Integer
  putStr "Mes (1-12): "
  m <- fmap read getLine :: IO Int
  putStrLn ""
  putStrLn (resumenMensual y m rs)
  let ranking = categoriasMayorGasto rs
  if null ranking
    then putStrLn "Sin datos de gastos."
    else do
      putStrLn "── Categorías con mayor gasto ──"
      mapM_ (\(cat, total) ->
               putStrLn ("  " ++ mostrarCategoria cat ++ ": " ++ formatMonto total))
            ranking
  putStrLn "\n── Comparación de períodos ──"
  putStr "Período 1 - Año: "
  y1 <- fmap read getLine :: IO Integer
  putStr "Período 1 - Mes: "
  m1 <- fmap read getLine :: IO Int
  putStr "Período 2 - Año: "
  y2 <- fmap read getLine :: IO Integer
  putStr "Período 2 - Mes: "
  m2 <- fmap read getLine :: IO Int
  putStrLn (comparacionPeriodos (y1, m1) (y2, m2) rs)

-- ─── Helpers de input ─────────────────────────────────────────────────────────

pedirTipo :: IO TipoRegistro
pedirTipo = do
  putStrLn "  Tipo de registro:"
  putStrLn "    1. Ingreso"
  putStrLn "    2. Gasto"
  putStrLn "    3. Ahorro"
  putStrLn "    4. Inversión"
  putStr "  Opción: "
  op <- getLine
  case op of
    "1" -> return Ingreso
    "2" -> return Gasto
    "3" -> return Ahorro
    "4" -> return Inversion
    _   -> putStrLn "  Opción inválida, intente de nuevo." >> pedirTipo

pedirCategoria :: IO Categoria
pedirCategoria = do
  putStrLn "  Categoría:"
  putStrLn "    1. Alimentación"
  putStrLn "    2. Transporte"
  putStrLn "    3. Vivienda"
  putStrLn "    4. Entretenimiento"
  putStrLn "    5. Salud"
  putStrLn "    6. Educación"
  putStrLn "    7. Otro"
  putStr "  Opción: "
  op <- getLine
  case op of
    "1" -> return Alimentacion
    "2" -> return Transporte
    "3" -> return Vivienda
    "4" -> return Entretenimiento
    "5" -> return Salud
    "6" -> return Educacion
    "7" -> do
      putStr "  Nombre de la categoría: "
      nombre <- getLine
      return (Otro nombre)
    _   -> putStrLn "  Opción inválida, intente de nuevo." >> pedirCategoria

pedirFecha :: IO Day
pedirFecha = do
  linea <- getLine
  let parts = splitOn '-' linea
  if length parts /= 3
    then do
      putStr "  Formato inválido. Use YYYY-MM-DD: "
      pedirFecha
    else do
      let y = read (parts !! 0) :: Integer
          m = read (parts !! 1) :: Int
          d = read (parts !! 2) :: Int
      return (fromGregorian y m d)

-- ─── Helpers de display ───────────────────────────────────────────────────────

mostrarCategoria :: Categoria -> String
mostrarCategoria Alimentacion    = "Alimentación"
mostrarCategoria Transporte      = "Transporte"
mostrarCategoria Vivienda        = "Vivienda"
mostrarCategoria Entretenimiento = "Entretenimiento"
mostrarCategoria Salud           = "Salud"
mostrarCategoria Educacion       = "Educación"
mostrarCategoria (Otro s)        = "Otro (" ++ s ++ ")"

mostrarTipo :: TipoRegistro -> String
mostrarTipo Ingreso   = "Ingreso"
mostrarTipo Gasto     = "Gasto"
mostrarTipo Ahorro    = "Ahorro"
mostrarTipo Inversion = "Inversión"

formatMonto :: Double -> String
formatMonto n = "₡" ++ show (fromIntegral (round n :: Int) :: Int)

mostrarRegistro :: Registro -> String
mostrarRegistro r = unlines
  [ "┌─ #" ++ show (registroId r) ++ " ─────────────────────────"
  , "│  Tipo:        " ++ mostrarTipo (tipoRegistro r)
  , "│  Monto:       " ++ formatMonto (monto r)
  , "│  Categoría:   " ++ mostrarCategoria (categoria r)
  , "│  Fecha:       " ++ show (fecha r)
  , "│  Descripción: " ++ descripcion r
  , "│  Etiquetas:   " ++ if null (etiquetas r) then "-"
                          else intercalate ", " (etiquetas r)
  , "└────────────────────────────────────────"
  ]

mostrarPresupuesto :: [Registro] -> Presupuesto -> String
mostrarPresupuesto rs p =
  let gastado  = gastoRealPorCategoria (presCategoria p) rs
      restante = presLimite p - gastado
      estado   = if gastado > presLimite p then "  EXCEDIDO" else " "
  in "  " ++ mostrarCategoria (presCategoria p) ++
     " | Límite: " ++ formatMonto (presLimite p) ++
     " | Gastado: " ++ formatMonto gastado ++
     " | Restante: " ++ formatMonto restante ++ estado

mostrarRegla :: Regla -> String
mostrarRegla r =
  let tipoStr = case reglaTipo r of
                  AlertaPresupuesto -> "Alerta si gasto >"
                  AdvertenciaAhorro -> "Advertencia si ahorro <"
  in mostrarCategoria (reglaCategoria r) ++ " - " ++ tipoStr ++
     " " ++ formatMonto (reglaLimite r)

-- ─── Utilidades ───────────────────────────────────────────────────────────────

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn sep (c:cs)
  | c == sep  = "" : splitOn sep cs
  | otherwise = case splitOn sep cs of
                  []     -> [[c]]
                  (w:ws) -> (c:w) : ws
