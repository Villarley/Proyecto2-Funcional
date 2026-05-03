module Types
  ( TipoRegistro(..)
  , Categoria(..)
  , Etiqueta
  , Registro(..)
  , Presupuesto(..)
  , Regla(..)
  , TipoAlerta(..)
  , Alerta(..)
  ) where

import Data.Time (Day)

type Etiqueta = String

data TipoRegistro
  = Ingreso
  | Gasto
  | Ahorro
  | Inversion
  deriving (Show, Read, Eq)

data Categoria
  = Alimentacion
  | Transporte
  | Vivienda
  | Entretenimiento
  | Salud
  | Educacion
  | Otro String
  deriving (Show, Read, Eq, Ord)

data Registro = Registro
  { registroId     :: Int
  , tipoRegistro   :: TipoRegistro
  , monto          :: Double
  , categoria      :: Categoria
  , fecha          :: Day
  , descripcion    :: String
  , etiquetas      :: [Etiqueta]
  } deriving (Show, Read, Eq)

data Presupuesto = Presupuesto
  { presCategoria :: Categoria
  , presLimite    :: Double
  } deriving (Show, Read, Eq)

data TipoAlerta = AlertaPresupuesto | AdvertenciaAhorro
  deriving (Show, Read, Eq)

data Alerta = Alerta
  { tipoAlerta :: TipoAlerta
  , mensaje    :: String
  } deriving (Show, Read, Eq)

data Regla = Regla
  { reglaCategoria :: Categoria
  , reglaLimite    :: Double
  , reglaTipo      :: TipoAlerta
  } deriving (Show, Read, Eq)
