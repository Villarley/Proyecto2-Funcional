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
import System.IO (hFlush, stdout)

-- inicio del programa

main :: IO ()
main = do
  registros    <- cargarRegistros
  presupuestos <- cargarPresupuestos
  reglas       <- cargarReglas
  refReg   <- newIORef registros
  refPres  <- newIORef presupuestos
  refRegl  <- newIORef reglas
  putStrLn "--------------------------------------"
  putStrLn "|  Sistema de Finanzas Personales    |"
  putStrLn "--------------------------------------"
  menuPrincipal refReg refPres refRegl

-- menú principal

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
  op <- promptLine "Opción: "
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
  putStrLn "Datos guardados. Vuelva pronto!"

-- menú de registros

menuRegistros :: IORef [Registro] -> IO ()
menuRegistros refReg = do
  putStrLn ""
  putStrLn "── REGISTROS ──"
  putStrLn "  1. Agregar registro"
  putStrLn "  2. Listar todos"
  putStrLn "  3. Filtrar registros"
  putStrLn "  4. Eliminar registro"
  putStrLn "  0. Volver"
  op <- promptLine "Opción: "
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
  mTipo <- pedirTipo
  case mTipo of
    Nothing -> putStrLn "Operación cancelada."
    Just tipo -> do
      mont <- fmap read (promptLine "Monto: ") :: IO Double
      mCat <- pedirCategoria
      case mCat of
        Nothing -> putStrLn "Operación cancelada."
        Just cat -> do
          putStr "Fecha (YYYY-MM-DD): "
          hFlush stdout
          dia  <- pedirFecha
          desc <- promptLine "Descripción: "
          etStr <- promptLine "Etiquetas (separadas por coma, o Enter para ninguna): "
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
  putStrLn "  0. Volver"
  op <- promptLine "Opción: "
  rs <- readIORef refReg
  case op of
    "0" -> return ()
    "1" -> do
      mTipo <- pedirTipo
      case mTipo of
        Nothing -> return ()
        Just tipo -> do
          let filtrados = filtrarPorTipo tipo rs
          if null filtrados then putStrLn "Sin resultados."
          else mapM_ (putStrLn . mostrarRegistro) filtrados
    "2" -> do
      mCat <- pedirCategoria
      case mCat of
        Nothing -> return ()
        Just cat -> do
          let filtrados = filtrarPorCategoria cat rs
          if null filtrados then putStrLn "Sin resultados."
          else mapM_ (putStrLn . mostrarRegistro) filtrados
    "3" -> do
      putStr "Fecha inicio (YYYY-MM-DD): "
      hFlush stdout
      desde <- pedirFecha
      putStr "Fecha fin    (YYYY-MM-DD): "
      hFlush stdout
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
      rid <- fmap read (promptLine "ID a eliminar: ") :: IO Int
      writeIORef refReg (eliminarRegistro rid rs)
      putStrLn " Registro eliminado."

-- menú de presupuestos

menuPresupuestos :: IORef [Presupuesto] -> IORef [Registro] -> IO ()
menuPresupuestos refPres refReg = do
  putStrLn ""
  putStrLn "── PRESUPUESTOS ──"
  putStrLn "  1. Agregar presupuesto"
  putStrLn "  2. Ver presupuestos actuales"
  putStrLn "  3. Ver alertas"
  putStrLn "  4. Eliminar presupuesto"
  putStrLn "  0. Volver"
  op <- promptLine "Opción: "
  case op of
    "1" -> agregarPresupuestoMenu refPres >> menuPresupuestos refPres refReg
    "2" -> listarPresupuestos refPres refReg >> menuPresupuestos refPres refReg
    "3" -> verAlertasPresupuesto refPres refReg >> menuPresupuestos refPres refReg
    "4" -> eliminarPresupuestoMenu refPres >> menuPresupuestos refPres refReg
    "0" -> return ()
    _   -> putStrLn "Opción inválida." >> menuPresupuestos refPres refReg

agregarPresupuestoMenu :: IORef [Presupuesto] -> IO ()
agregarPresupuestoMenu refPres = do
  mCat <- pedirCategoria
  case mCat of
    Nothing -> putStrLn "Operación cancelada."
    Just cat -> do
      lim <- fmap read (promptLine "Límite mensual: ") :: IO Double
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
      idx <- fmap read (promptLine "Número a eliminar: ") :: IO Int
      let nuevos = [ p | (i, p) <- zip ([1..] :: [Int]) ps, i /= idx ]
      writeIORef refPres nuevos
      putStrLn " Presupuesto eliminado."

-- menú de reglas

menuReglas :: IORef [Regla] -> IORef [Registro] -> IO ()
menuReglas refRegl refReg = do
  putStrLn ""
  putStrLn "── REGLAS Y ALERTAS ──"
  putStrLn "  1. Agregar regla"
  putStrLn "  2. Ver reglas"
  putStrLn "  3. Evaluar reglas"
  putStrLn "  4. Eliminar regla"
  putStrLn "  0. Volver"
  op <- promptLine "Opción: "
  case op of
    "1" -> agregarReglaMenu refRegl  >> menuReglas refRegl refReg
    "2" -> listarReglas refRegl      >> menuReglas refRegl refReg
    "3" -> evaluarReglasMenu refRegl refReg >> menuReglas refRegl refReg
    "4" -> eliminarReglaMenu refRegl >> menuReglas refRegl refReg
    "0" -> return ()
    _   -> putStrLn "Opción inválida." >> menuReglas refRegl refReg

agregarReglaMenu :: IORef [Regla] -> IO ()
agregarReglaMenu refRegl = do
  mCat <- pedirCategoria
  case mCat of
    Nothing -> putStrLn "Operación cancelada."
    Just cat -> do
      lim <- fmap read (promptLine "Umbral (monto): ") :: IO Double
      putStrLn "Tipo de alerta:"
      putStrLn "  1. Alerta de presupuesto (si gasto > umbral)"
      putStrLn "  2. Advertencia de ahorro (si ahorro < umbral)"
      putStrLn "  0. Cancelar"
      tOp <- promptLine "Opción: "
      case tOp of
        "0" -> putStrLn "Operación cancelada."
        _ -> do
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
      idx <- fmap read (promptLine "Número a eliminar: ") :: IO Int
      writeIORef refRegl [ r | (i, r) <- zip ([1..] :: [Int]) rs, i /= idx ]
      putStrLn " Regla eliminada."

-- menú de análisis

menuAnalisis :: IORef [Registro] -> IO ()
menuAnalisis refReg = do
  rs <- readIORef refReg
  putStrLn ""
  putStrLn "── ANÁLISIS FINANCIERO ──"
  putStrLn "  0. Volver"
  putStrLn "  1. Ver análisis"
  op <- promptLine "Opción: "
  case op of
    "0" -> return ()
    "1" -> mostrarAnalisis rs
    _   -> putStrLn "Opción inválida." >> menuAnalisis refReg

mostrarAnalisis :: [Registro] -> IO ()
mostrarAnalisis rs = do
  putStrLn "\n[Flujo de caja mensual]"
  let flujo = flujoCajaMensual rs
  if null flujo then putStrLn "  Sin datos."
  else mapM_ (\(m, f) -> putStrLn ("  " ++ m ++ ": " ++ formatMonto f)) flujo
  putStrLn "\n[Tendencia de gastos por mes]"
  let tend = tendenciaGasto rs
  if null tend then putStrLn "  Sin datos."
  else mapM_ (\(m, t) -> putStrLn ("  " ++ m ++ ": " ++ formatMonto t)) tend
  putStrLn "\n[Gasto promedio mensual (proyección)]"
  putStrLn ("  " ++ formatMonto (proyeccionGastos rs))
  putStrLn "\n[Desglose de gastos por categoría]"
  let breakdown = gastosPorCategoria rs
  if null breakdown then putStrLn "  Sin datos."
  else mapM_ (\(cat, tot) ->
                putStrLn ("  " ++ mostrarCategoria cat ++ ": " ++ formatMonto tot))
             breakdown
  putStrLn "\n[Categoría con mayor impacto]"
  case categoriaMayorImpacto rs of
    Nothing  -> putStrLn "  Sin datos."
    Just cat -> putStrLn ("  " ++ mostrarCategoria cat)

-- menú de simulación

menuSimulacion :: IORef [Registro] -> IO ()
menuSimulacion refReg = do
  rs <- readIORef refReg
  putStrLn ""
  putStrLn "── SIMULACIÓN FINANCIERA ──"
  putStrLn "  0. Volver"
  putStrLn "  1. Ejecutar simulación"
  op <- promptLine "Opción: "
  case op of
    "0" -> return ()
    "1" -> ejecutarSimulacion rs
    _   -> putStrLn "Opción inválida." >> menuSimulacion refReg

ejecutarSimulacion :: [Registro] -> IO ()
ejecutarSimulacion rs = do
  putStrLn ("  Gasto mensual promedio actual: " ++ formatMonto (promedioMensual Gasto rs))
  p <- fmap read (promptLine "\nPorcentaje de reducción de gastos (%): ") :: IO Double
  let (gastoNuevo, ahorro) = simularReduccionGastos p rs
  putStrLn ("  Gasto mensual reducido a:      " ++ formatMonto gastoNuevo)
  putStrLn ("  Ahorro extra por mes:           " ++ formatMonto ahorro)
  n <- fmap read (promptLine "\nProyectar ahorro acumulado a cuántos meses: ") :: IO Int
  putStrLn ""
  mapM_ (\(mes, ac) -> putStrLn ("  Mes " ++ show mes ++ ": " ++ formatMonto ac))
        (proyeccionAhorroAcumulado ahorro n)

-- menú de reportes

menuReportes :: IORef [Registro] -> IO ()
menuReportes refReg = do
  rs <- readIORef refReg
  putStrLn ""
  putStrLn "── REPORTES ──"
  putStrLn "  0. Volver"
  putStrLn "  1. Generar reportes"
  op <- promptLine "Opción: "
  case op of
    "0" -> return ()
    "1" -> ejecutarReportes rs
    _   -> putStrLn "Opción inválida." >> menuReportes refReg

ejecutarReportes :: [Registro] -> IO ()
ejecutarReportes rs = do
  y <- fmap read (promptLine "Año (ej. 2026): ") :: IO Integer
  m <- fmap read (promptLine "Mes (1-12): ") :: IO Int
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
  y1 <- fmap read (promptLine "Período 1 - Año: ") :: IO Integer
  m1 <- fmap read (promptLine "Período 1 - Mes: ") :: IO Int
  y2 <- fmap read (promptLine "Período 2 - Año: ") :: IO Integer
  m2 <- fmap read (promptLine "Período 2 - Mes: ") :: IO Int
  putStrLn (comparacionPeriodos (y1, m1) (y2, m2) rs)

-- helpers

promptLine :: String -> IO String
promptLine msg = putStr msg >> hFlush stdout >> getLine

pedirTipo :: IO (Maybe TipoRegistro)
pedirTipo = do
  putStrLn "  Tipo de registro:"
  putStrLn "    1. Ingreso"
  putStrLn "    2. Gasto"
  putStrLn "    3. Ahorro"
  putStrLn "    4. Inversión"
  putStrLn "    0. Cancelar"
  op <- promptLine "  Opción: "
  case op of
    "0" -> return Nothing
    "1" -> return (Just Ingreso)
    "2" -> return (Just Gasto)
    "3" -> return (Just Ahorro)
    "4" -> return (Just Inversion)
    _   -> putStrLn "  Opción inválida, intente de nuevo." >> pedirTipo

pedirCategoria :: IO (Maybe Categoria)
pedirCategoria = do
  putStrLn "  Categoría:"
  putStrLn "    1. Alimentación"
  putStrLn "    2. Transporte"
  putStrLn "    3. Vivienda"
  putStrLn "    4. Entretenimiento"
  putStrLn "    5. Salud"
  putStrLn "    6. Educación"
  putStrLn "    7. Otro"
  putStrLn "    0. Cancelar"
  op <- promptLine "  Opción: "
  case op of
    "0" -> return Nothing
    "1" -> return (Just Alimentacion)
    "2" -> return (Just Transporte)
    "3" -> return (Just Vivienda)
    "4" -> return (Just Entretenimiento)
    "5" -> return (Just Salud)
    "6" -> return (Just Educacion)
    "7" -> do
      nombre <- promptLine "  Nombre de la categoría: "
      return (Just (Otro nombre))
    _   -> putStrLn "  Opción inválida, intente de nuevo." >> pedirCategoria

pedirFecha :: IO Day
pedirFecha = do
  linea <- getLine
  let parts = splitOn '-' linea
  if length parts /= 3
    then do
      putStr "  Formato inválido. Use YYYY-MM-DD: "
      hFlush stdout
      pedirFecha
    else do
      let y = read (parts !! 0) :: Integer
          m = read (parts !! 1) :: Int
          d = read (parts !! 2) :: Int
      return (fromGregorian y m d)

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
  [ "#" ++ show (registroId r)
  , "Tipo:        " ++ mostrarTipo (tipoRegistro r)
  , "Monto:       " ++ formatMonto (monto r)
  , "Categoría:   " ++ mostrarCategoria (categoria r)
  , "Fecha:       " ++ show (fecha r)
  , "Descripción: " ++ descripcion r
  , "Etiquetas:   " ++ if null (etiquetas r) then "-"
                          else intercalate ", " (etiquetas r)
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

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn sep (c:cs)
  | c == sep  = "" : splitOn sep cs
  | otherwise = case splitOn sep cs of
                  []     -> [[c]]
                  (w:ws) -> (c:w) : ws
