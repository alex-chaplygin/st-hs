module Compiler (meaning) where
import Control.Applicative
import qualified Control.Monad.State as S
import Types
-- виды переменных
data Var = Local Int Int -- ссылка на локальную переменную по 2 координатам 
  | Global Int -- сылка на глобальную переменную

--globalEnv var = error $"Нет переменной " ++ (show var)
-- анализатор программы
meaning :: Object -> Env -> Bool -> S.StateT (FrameList, GlobalEnv) IO Code
meaning o@(NUM i) e t = meaningQuote o e t
meaning (LIST (SYMBOL "QUOTE":o:[])) e t = meaningQuote o e t
meaning (SYMBOL name) e t = meaningReference name e
meaning (LIST (SYMBOL "LAMBDA":(LIST args):body)) e t = meaningAbstraction args body e t
meaning (LIST (SYMBOL "IF":expr:t:f:[])) e t' = meaningAlternative expr t f e t'
meaning (LIST (SYMBOL "BEGIN":s)) e t = meaningSequence s e t
meaning (LIST (SYMBOL "SETQ":SYMBOL var:exp:[])) e t = meaningAssignment var exp e t
meaning (LIST (SYMBOL "CAR":l:[])) e t = meaning l e False >>= \c -> return$CAR c 
meaning (LIST (SYMBOL "+":n:n2:[])) e t = do
  a1 <- meaning n e False
  a2 <- meaning n2 e False
  return $ ADD a1 a2
meaning (LIST (SYMBOL "*":n:n2:[])) e t = do
  a1 <- meaning n e False
  a2 <- meaning n2 e False
  return $ MUL a1 a2
meaning (LIST (car:cdr)) e t = meaningApplication car cdr e t
-- цитирование
meaningQuote o e t = return $ CONST o
-- чтение переменной
meaningReference :: String -> Env -> S.StateT (FrameList, GlobalEnv) IO Code
meaningReference name env = do
  (_, glEnv) <- S.get
  let v = case kindVar env name glEnv of
             Nothing -> error "Unknown var"
             Just (Global i) -> GLOBAL i 
             Just (Local i j) -> if i == 0 then VAR_SH j else VAR_DEEP i j
  return v
-- ветвление
meaningAlternative e1 e2 e3 env t = do
  m1 <- meaning e1 env False
  m2 <- meaning e2 env t
  m3 <- meaning e3 env t
  return $ IF m1 m2 m3
-- последовательность
meaningSequence (o:[]) e t = meaning o e t
meaningSequence (o:t) e t' = do
  m1 <- meaning o e False
  mt <- meaningSequence t e t'
  return $ SEQ m1 mt
-- присваивание
meaningAssignment var expr env t = do
  m <- meaning expr env False
  (fr, glEnv) <- S.get
  case kindVar env var glEnv of
    Nothing -> do
      S.put (fr, glEnv ++ [(var, NUM 0)])
      return $ SET_GLOBAL (length glEnv) m
    Just (Global i) -> return $ SET_GLOBAL i m
    Just (Local i j) -> return $ if i == 0 then SET_VAR_SH j m else SET_VAR_DEEP i j m
-- применение функции
meaningApplication f args env t = do
  m <- meaning f env False
  vals <- meaningArgs args env (length args) False
  return $ if t then TAIL_CALL m vals else CALL m vals
-- вычисление аргументов
meaningArgs [] _ size _ = return $ ALLOC size
meaningArgs (car:cdr) env size t = do
  m <- meaning car env False
  mt <- meaningArgs cdr env size t
  let idx = size - length cdr - 1
  return $ STORE m mt idx
-- абстракция
meaningAbstraction args body env t = do
  let arity = length args
      env' = [map (\a@(SYMBOL s)->s) args] ++ env
  m <- meaningSequence body env' True
  return $ CLOSURE m arity
-- рассчет индексов переменных для кадров активации
kindVar env name globEnv = localVar env 0 <|> globalVar where
  localVar [] _ = Nothing
  localVar (car:cdr) i = scan car 0 where
    scan [] _ = localVar cdr (i + 1)
    scan (h:t) j = if name == h then Just (Local i j) else scan t $ j + 1
  globalVar = scan 0 globEnv where
    scan _ [] = Nothing
    scan i ((n,v):cdr) = if n == name then Just (Global i) else scan (i + 1) cdr
