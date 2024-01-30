module Compiler (meaning) where
import Types
-- виды переменных
data Var = Local Int Int -- ссылка на локальную переменную по 2 координатам 
  | Global Int -- сылка на глобальную переменную

-- глобальное окружение - пустое в начале
globalEnv = ["T", "NIL"]
--globalEnv var = error $"Нет переменной " ++ (show var)
-- анализатор программы
meaning :: Object -> Env -> Bool -> Code
meaning o@(NUM i) e t = meaningQuote o e t
meaning (LIST (SYMBOL "QUOTE":o:[])) e t = meaningQuote o e t
meaning (SYMBOL name) e t = meaningReference name e
meaning (LIST (SYMBOL "LAMBDA":(LIST args):body)) e t = meaningAbstraction args body e t
meaning (LIST (SYMBOL "IF":expr:t:f:[])) e t' = meaningAlternative expr t f e t'
meaning (LIST (SYMBOL "BEGIN":s)) e t = meaningSequence s e t
meaning (LIST (SYMBOL "SETQ":SYMBOL var:exp:[])) e t = meaningAssignment var exp e t
meaning (LIST (SYMBOL "CAR":l:[])) e t = CAR $ meaning l e False
meaning (LIST (SYMBOL "+":n:n2:[])) e t = ADD (meaning n e False) (meaning n2 e False)
meaning (LIST (SYMBOL "*":n:n2:[])) e t = MUL (meaning n e False) (meaning n2 e False)
meaning (LIST (car:cdr)) e t = meaningApplication car cdr e t
-- цитирование
meaningQuote o e t = CONST o
-- чтение переменной
meaningReference name env = case kindVar env 0 name of
                              Nothing -> error "Unknown var"
                              Just (Global i) -> GLOBAL i 
                              Just (Local i j) -> if i == 0 then VAR_SH j else VAR_DEEP i j
-- рассчет индексов локальных переменных для кадров активации
kindVar [] _ name = scan' 0 globalEnv where
  scan' _ [] = Nothing
  scan' i (car:cdr) = if car == name then Just (Global i) else scan' (i + 1) cdr
kindVar (car:cdr) i name = scan car 0 where
  scan [] _ = kindVar cdr (i + 1) name
  scan (h:t) j = if name == h then Just (Local i j) else scan t $ j + 1
-- ветвление
meaningAlternative e1 e2 e3 env t = let m1 = meaning e1 env False
                                        m2 = meaning e2 env t
                                        m3 = meaning e3 env t in
                                      IF m1 m2 m3
-- последовательность
meaningSequence (o:[]) e t = meaning o e t
meaningSequence (o:t) e t' = let m1 = meaning o e False
                                 mt = meaningSequence t e t' in SEQ m1 mt
-- присваивание
meaningAssignment var expr env t = let m = meaning expr env False in
  case kindVar env 0 var of
    Nothing -> error "Unknown var"
    Just (Global i) -> SET_GLOBAL i m
    Just (Local i j) -> if i == 0 then SET_VAR_SH j m else SET_VAR_DEEP i j m
-- применение функции
meaningApplication f args env t = let m = meaning f env False
                                      vals = meaningArgs args env (length args) False in if t then TAIL_CALL m vals else CALL m vals
-- вычисление аргументов
meaningArgs [] _ size _ = ALLOC size
meaningArgs (car:cdr) env size t = let m = meaning car env False
                                       mt = meaningArgs cdr env size t
                                       idx = size - length cdr - 1 in
                                     STORE m mt idx
-- абстракция
meaningAbstraction args body env t = let arity = length args
                                         env' = [map (\a@(SYMBOL s)->s) args] ++ env
                                         m = meaningSequence body env' True in
                                       CLOSURE m arity
