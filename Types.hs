module Types (Object(..), Code(..), Env, GlobalEnv, FrameList) where

import Data.List
import Data.Array
import qualified Data.Vector as V

data Object = SYMBOL String
  | NUM Int
  | LIST [Object]
  deriving (Eq)
-- скомпилированный код
data Code = CONST Object -- константа
  | VAR_SH Int -- локальный аргумент
  | VAR_DEEP Int Int -- аргумент из замыкания
  | GLOBAL Int -- глобальная переменная
  | IF Code Code Code -- условный оператор
  | SEQ Code Code -- последовательность
  | SET_VAR_SH Int Code -- установка локального аргумента
  | SET_VAR_DEEP Int Int Code -- установка аргумента из замыкания
  | SET_GLOBAL Int Code -- установка глобальной переменной
  | CLOSURE Code Int -- замыкание
  | TAIL_CALL Code Code -- хвостовой вызов функции
  | CALL Code Code -- обычный вызов функции
  | ALLOC Int -- создание кадра активации (размер)
  | STORE Code Code Int -- сохранение значения аргумента в кадр активации
  | PRIM0 Int -- вызовы примитивов
  | PRIM1 Int Code
  | PRIM2 Int Code Code
  deriving (Show, Eq)

type Env = [[String]] -- окружение - список кадров из переменных
type GlobalEnv = [(String, Code)] -- глобальное окружение
type FrameList = [V.Vector Object] -- записи активаций
type Primitive = (String, Int, [Object]->Object) -- примитивные функции

instance Show Object where
  show (SYMBOL s) = s
  show (NUM i) = (show i)
  show (LIST []) = "NIL"
  show (LIST l) = "(" ++ (concat $ intersperse " " $ map show l) ++ ")"
