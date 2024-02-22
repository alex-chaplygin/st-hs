-- Базовый интерпретатор Лисп с одним окружением
import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Hashable
import qualified Control.Monad.State as S

data Object = SYMBOL String
  | NUM Int
  | LIST [Object]
  | LAMBDA Object [Object] Env -- функция параметры тело окружение
  | CONT Cont -- объект - продолжение

type Var = String -- переменная
type Addr = Int -- адрес памяти
type Env = Var -> Addr -- окружение хранит адреса для переменных
type Mem = Addr -> Object -- память хранит объекты
type Cont = Object -> Env -> Mem -> (Object, Env, Mem)-- продолжение

instance Show Object where
  show (SYMBOL s) = s
  show (NUM i) = (show i)
  show (LIST []) = "NIL"
  show (LIST l) = "(" ++ (concat $ intersperse " " $ map show l) ++ ")"
  show (LAMBDA args body e) = "LAMBDA " ++ (show args) ++ " " ++ (concat $ intersperse " " $ map show body)
  show (CONT s) = "CONT"

isNotDigit c = c < '0' || c > '9'
isMySymbol c = isSymbol c || c == '*' || c == '/' || c == '+' || c == '-'

number :: ReadP Object
number = do
  skipSpaces
  s <- munch1 isDigit
  return $ NUM (read s)

mychar c = do {skipSpaces; char c}

atomFirst c = (isAlpha c || isMySymbol c) && c /= '(' && c /= ')'
atomSym c = (isAlphaNum c || isMySymbol c) && c /= '(' && c /= ')'

qexpr :: ReadP Object
symbol = do {skipSpaces; c <- satisfy atomFirst; s <- munch atomSym; return $ SYMBOL $ map toUpper (c:s)}
atom = symbol +++ number
obj = atom +++ qexpr
sexpr =  atom +++ do {mychar '(' ; e <- many obj; mychar ')'; return $ LIST e}
qexpr = do {mychar '\''; e <- sexpr; return $ LIST $ (SYMBOL "QUOTE") : [e]
           } +++ sexpr
parse s = readP_to_S obj s
-- глобальное окружение - пустое в начале
globalEnv :: Env
globalEnv "T" = 1
globalEnv "NIL" = 2
globalEnv var = error $"Нет переменной " ++ (show var)
-- начальная память
initMem 0 = NUM 2 -- последняя свободная ячейка
initMem 1 = SYMBOL "T"
initMem 2 = LIST []
initMem a = error $ "Неверный адрес " ++ (show a)

eval :: Object -> Env -> Cont -> Mem -> (Object, Env, Mem)
-- вычисление последовательности
evalBegin :: [Object] -> Env -> Cont -> Mem -> (Object, Env, Mem)
evalBegin (o:[]) env cc mem = eval o env cc mem
evalBegin (o:t) env cc mem = eval o env (\_ e m -> evalBegin t e cc m) mem
-- логические значения
true :: a -> a -> a
true = \x _ -> x
false = \_ y -> y
-- приведение к логическому типу
toBool :: Object -> (a -> a -> a)
toBool (SYMBOL "T") = true
toBool (LIST []) = false
toBool _ = error "Invalid bool"
-- расширение памяти, новый адрес становится последним
extendMem :: Mem -> Addr -> Object -> Mem
extendMem m a o = \adr -> if adr == 0 then NUM a else if a == adr then o else m adr
-- расширение окружения (создание новой переменной)
extendEnv :: Env -> Mem -> Var -> Env
extendEnv e m v = \var -> if v == var then let (NUM n) = m 0 in n + 1 else e var
-- расширение окружения при вызове функции
extendFEnv :: Env -> Mem -> [Object] -> Env
extendFEnv e m = let NUM n = m 0 in fst . foldl (\(e', n) (SYMBOL v) -> (\var -> if v == var then n + 1 else e' var, n + 1)) (e, n)
-- расширение памяти при применении функции
extendFMem :: Mem -> [Object] -> Mem
extendFMem mem = let NUM n = mem 0 in fst . foldl (\(m, n) v -> (extendMem m (n + 1) v, n + 1)) (mem, n)
-- применение функции
applyFun :: Object -> [Object] -> Env -> Cont -> Mem -> (Object, Env, Mem)
applyFun (LAMBDA (LIST params) body fenv) args env c mem = (\vals e m -> evalBegin body (extendFEnv e m params) (\o _ _->c o env m) $ extendFMem m vals) (map (\x->(\(x, _, _) -> x) $ eval x env c mem) args) fenv mem
applyFun (CONT cc) args env c mem = cc (head args) env mem

eval (NUM i) e c m = c (NUM i) e m
eval (LIST []) e c m = c (LIST []) e m
eval (CONT cc) e c m = c (CONT cc) e m -- цитируем продолжение
eval (SYMBOL name) env cont mem = cont (mem $ env name) env mem
eval (LIST (SYMBOL "QUOTE":cdr)) e c m = c (head cdr) e m
eval (LIST (SYMBOL "BEGIN":cdr)) env cc mem = evalBegin cdr env cc mem
eval (LIST (SYMBOL "IF":expr:t:f:[])) env cc mem = eval expr env
  (\e _ m -> (\p -> p (eval t env cc m) (eval f env cc m)) $ toBool e) mem
eval (LIST (SYMBOL "SETQ":SYMBOL var:expr:[])) env cc mem = eval expr env (\v e m -> let e' = extendEnv env m var in cc v e' $ extendMem m (e' var) v) mem
eval (LIST (SYMBOL "LAMBDA":args:body)) env c mem = c (LAMBDA args body env) env mem
eval (LIST (SYMBOL "CALL/CC":LIST (SYMBOL "LAMBDA":args:body):[])) env cc mem = applyFun (LAMBDA args body env) [CONT cc] env cc mem
eval (LIST (SYMBOL "CONS":car:cdr:[])) e c m =
  let (car', _, _) = eval car e c m
      (LIST cdr', _, _) = eval cdr e c m in c (LIST (car':cdr')) e m
eval (LIST (SYMBOL "CAR":l:[])) e c m = let (LIST l', _, _) = eval l e c m in c (head l') e m
eval (LIST (SYMBOL "CDR":l:[])) e c m = let (LIST l', _, _) = eval l e c m in c (LIST $ tail l') e m
eval (LIST (car:cdr)) env c mem = eval car env (\o e m -> applyFun o cdr e c m) mem
eval _ _ _ _ = error "Eval error"

process :: S.StateT (Env, Mem) IO ()
process = do
  (e, mem) <- S.get -- получаем текущее окружение
  S.liftIO $ putStr "> "
  str <- S.liftIO $ getLine
  if str == "q" then return ()
    else do
    let ob = parse str
    let (res, e', mem') = eval (fst $ last $ ob) e (\o e m -> (o, e, m)) mem
    S.liftIO $ putStrLn (show res)
    S.put (e', mem')
    process

repl :: IO ()
repl = do
  res <- S.runStateT process (globalEnv, initMem)
  return ()
main = repl
