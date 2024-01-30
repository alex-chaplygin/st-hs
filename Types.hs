module Types (Object(..), Code(..), Env) where

import Data.List

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
  | CAR Code
  | ADD Code Code
  | MUL Code Code
  deriving (Show)

type Env = [[String]] -- окружение - список кадров из переменных

instance Show Object where
  show (SYMBOL s) = s
  show (NUM i) = (show i)
  show (LIST []) = "NIL"
  show (LIST l) = "(" ++ (concat $ intersperse " " $ map show l) ++ ")"
