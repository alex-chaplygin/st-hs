module Interpreter (meaning) where
import Types
import Env
-- анализатор программы
meaning :: Object -> Env -> FrameList -> Cont -> (Object, Env)
meaning o@(NUM _) e = meaningQuote o e
meaning (LIST (SYMBOL "QUOTE":o:[])) e = meaningQuote o e
--meaning (SYMBOL name) e t = meaningReference name e
--meaning (LIST (SYMBOL "LAMBDA":(LIST args):body)) e t = meaningAbstraction args body e t
--meaning (LIST (SYMBOL "IF":expr:t:f:[])) e t' = meaningAlternative expr t f e t'
--meaning (LIST (SYMBOL "BEGIN":s)) e t = meaningSequence s e t
--meaning (LIST (SYMBOL "SETQ":SYMBOL var:exp:[])) e t = meaningAssignment var exp e t
--meaning (LIST (car:cdr)) e t = meaningApplication car cdr e t
-- цитирование
meaningQuote :: Object -> Env -> FrameList -> Cont -> (Object, Env)
meaningQuote o e = \sr k -> k o e
-- чтение переменной
--meaningReference name env = do
--  (glEnv,_) <- get
--  emit $ case kindVar env name glEnv of
--             Nothing -> error "Unknown var"
--             Just (Global i) -> GLOBAL i 
--             Just (Local i j) -> if i == 0 then VAR_SH j else VAR_DEEP i j
-- ветвление
--meaningAlternative e1 e2 e3 env t = do
--  meaning e1 env False
-- последовательность
--meaningSequence (o:[]) e t = meaning o e t
--meaningSequence (o:t) e t' = do
--  meaning o e False
--  meaningSequence t e t'
-- присваивание
--meaningAssignment var expr env t = do
--  meaning expr env False
--  (glEnv, code) <- get
--  case kindVar env var glEnv of
--    Nothing -> do
--      put $ (glEnv ++ [(var, NUM 0)], code)
--      emit $ SET_GLOBAL (length glEnv)
--    Just (Global i) -> emit $ SET_GLOBAL i
--    Just (Local i j) -> emit $ if i == 0 then SET_VAR_SH j else SET_VAR_DEEP i j
-- применение функции
--meaningApplication f args env t = do
--  case f of
--    SYMBOL n -> case isPrimitive n of
--      Just (i, arity) -> if arity /= length args then error "Incorrect arity"
--        else meaningPrimitive i arity args env t
--      Nothing -> go
--    _ -> go
--  where go = do
--          m <- meaning f env False
--          vals <- meaningArgs args env (length args) False
--          return $ if t then TAIL_CALL m vals else CALL m vals
-- вычисление аргументов
--meaningArgs [] _ size _ = return $ ALLOC size
--meaningArgs (car:cdr) env size t = do
--  m <- meaning car env False
--  mt <- meaningArgs cdr env size t
--  let idx = size - length cdr - 1
--  return $ STORE m mt idx
-- вызов примитива
--meaningPrimitive num arity args env t = case arity of
--  0 -> return $ PRIM0 num
--  1 -> do meaning (head args) env False >>= \m -> return $ PRIM1 num m
--  2 -> do
--    m1 <- meaning (head args) env False
--    m2 <- meaning (head $ tail args) env False
--    return $ PRIM2 num m1 m2
-- абстракция
--meaningAbstraction args body env t = do
--  let arity = length args
--      env' = [map (\a@(SYMBOL s)->s) args] ++ env
