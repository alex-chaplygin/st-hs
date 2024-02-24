module Env(Env, FrameList, Cont, deepFetch, deepUpdate, kindVar) where
import qualified Data.Vector as V
import Control.Applicative
import Types

type Env = [[String]] -- окружение - список кадров из переменных
type FrameList = [V.Vector Object] -- записи активаций
type Cont = Object -> Env -> (Object, Env)
-- виды переменных
data Var = Local Int Int -- ссылка на локальную переменную по 2 координатам 
  | Global Int -- сылка на глобальную переменную
-- извлечение значения переменной
deepFetch :: FrameList -> Int -> Int -> Object
deepFetch fr i j = if i == 0 then head fr V.! j else fr !! i V.! j
-- глубокое обновление переменной
deepUpdate :: FrameList -> Int -> Int -> Object -> FrameList
deepUpdate (car:cdr) 0 j v = (car V.// [(j, v)]) : cdr
deepUpdate (car:cdr) i j v = car : deepUpdate cdr (i-1) j v
-- рассчет индексов переменных для кадров активации
kindVar env name globEnv = localVar env 0 <|> globalVar where
  localVar [] _ = Nothing
  localVar (car:cdr) i = scan car 0 where
    scan [] _ = localVar cdr (i + 1)
    scan (h:t) j = if name == h then Just (Local i j) else scan t $ j + 1
  globalVar = scan 0 globEnv where
    scan _ [] = Nothing
    scan i ((n,v):cdr) = if n == name then Just (Global i) else scan (i + 1) cdr
