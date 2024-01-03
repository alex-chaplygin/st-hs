-- Базовый интерпретатор Лисп с одним окружением
import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Control.Monad.State as S

data Object = SYMBOL String
  | NUM Int
  | LIST [Object]
  | LAMBDA Object [Object] Environment -- функция параметры тело окружение
  deriving (Eq)

type Var = String -- переменная
type Addr = Int -- адрес памяти
type Env = Var -> Addr -- окружение хранит адреса для переменных
type Mem = Addr -> Object -- память хранит объекты
type Cont = Object -> Mem -> Object -- продолжение
type Fun = [Object] -> Cont -> Mem -> Object -- функция тело продолж память ->

data Result = OK Object
  | ERROR String
  deriving Eq

instance Show Object where
  show (SYMBOL s) = s
  show (NUM i) = (show i)
  show (LIST []) = "NIL"
  show (LIST l) = "(" ++ (concat $ intersperse " " $ map show l) ++ ")"
  show (LAMBDA args body env) = "LAMBDA " ++ (show args) ++ " " ++ (concat $ intersperse " " $ map show body) ++ "env = " ++ (show env)

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
globalEnv var = error $"Нет переменной " ++ (show var)

eval :: Object -> Env -> Cont -> Mem -> Object
-- вычисление последовательности
evalBegin :: [Object] -> Env -> Cont -> Mem -> Obj
evalBegin (o:[]) env cc mem = eval o env cc mem
evalBegin (o:t) env cc mem = eval o env (\_ m -> evalBegin t env cc m) mem
-- логические значения
true :: a -> a -> a
true = \x _ -> x
false = \_ y -> y
-- приведение к логическому типу
toBool :: Object -> (a -> b -> a)
toBool (SYMBOL "T") = true
toBool (LIST []) = false
toBool _ = error "Invalid bool"
-- расширение памяти
extendMem :: Mem -> Addr -> Object -> Mem
extendMem m a o = \adr -> if a == adr then o else m adr

eval (NUM i) _ _ _ = NUM i
eval (LIST []) _ _ _ = LIST []
eval (SYMBOL name) env cont mem = cont (mem $ env name) mem
eval (LIST (SYMBOL "BEGIN":cdr)) env cc mem = evalBegin cdr env cc mem
eval (LIST (SYMBOL "IF":expr:t:f:[])) env cc mem = eval expr env
  (\e m -> (\p -> p (eval t env cc m) (eval f env cc m)) $ toBool e) mem
eval (LIST (SYMBOL "SETQ":SYMBOL var:expr:[])) env cc mem = eval expr (\v m -> cc v $ extendMem m (env var) v) mem 

process :: S.StateT Environment IO ()
process = do
  e <- S.get -- получаем текущее окружение
  S.liftIO $ putStr "> "
  str <- S.liftIO $ getLine
  if str == "q" then return ()
    else do
    let ob = parse str
    if ob == [] then do
      S.liftIO $ putStrLn "Ошибка ввода"
      else do
      let (res, env) = eval e (fst $ last $ ob)
      S.liftIO $ putStrLn (show res)
      S.put env
    process

repl :: IO ()
repl = do
  let env = globalEnv
  res <- S.runStateT process env
  return ()
main = repl
