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
  | LAMBDA Object [Object] -- функция параметры тело окружение
  deriving (Eq)

type Frame = Array Int Object -- массив значений (запись активации)
data Env = Env -- окружение
  { nextEnv :: Env -- следующий кадр
  , frameEnv :: Frame -- значения аргументов при вызове функций
  }
type Cont = Object -> Env -> Mem -> (Object, Env, Mem)-- продолжение

data Result = OK Object
  | ERROR String
  deriving Eq

instance Show Object where
  show (SYMBOL s) = s
  show (NUM i) = (show i)
  show (LIST []) = "NIL"
  show (LIST l) = "(" ++ (concat $ intersperse " " $ map show l) ++ ")"
  show (LAMBDA args body) = "LAMBDA " ++ (show args) ++ " " ++ (concat $ intersperse " " $ map show body)

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

meaning :: Object -> Env -> Cont -> Mem -> (Object, Env, Mem)
-- вычисление последовательности
evalBegin :: [Object] -> Env -> Cont -> Mem -> (Object, Env, Mem)
evalBegin (o:[]) env cc mem = eval o env cc mem
evalBegin (o:t) env cc mem = eval o env (\_ e m -> evalBegin t e cc m) mem

-- расширение окружения (создание новой записи активации)
extendEnv :: Env -> Frame -> Env
extendEnv e fr = Env {frameEnv = fr, nextEnv = e}
-- глубокий поиск переменной - номер кадра - номер записи
deepFetch :: Env -> Int -> Int -> Object
deepFetch e 0 j = frameEnv e ! j
deepFetch e i j = deepFetch e $ i - 1 $ j
-- глубокое обновление переменной
deepUpdate :: Env -> Int -> Int -> Object -> Env
deepUpdate e 0 j v = Env {nextEnv = nextEnv e, frameEnv = frameEnv e // [(j, o)]}
deepUpdate e i j v = Env {frameEnv = frameEnv e, nextEnv = deepUpdate e$i-1$jv}

eval (NUM i) e c m = c (NUM i) e m
eval (LIST []) e c m = c (LIST []) e m
eval (SYMBOL name) env cont mem = cont (mem $ env name) env mem
eval (LIST (SYMBOL "QUOTE":cdr)) e c m = c (head cdr) e m
eval (LIST (SYMBOL "BEGIN":cdr)) env cc mem = evalBegin cdr env cc mem
eval (LIST (SYMBOL "IF":expr:t:f:[])) env cc mem = eval expr env
  (\e _ m -> (\p -> p (eval t env cc m) (eval f env cc m)) $ toBool e) mem
eval (LIST (SYMBOL "SETQ":SYMBOL var:expr:[])) env cc mem = eval expr env (\v e m -> let e' = extendEnv env m var in cc v e' $ extendMem m (e' var) v) mem
eval (LIST (SYMBOL "LAMBDA":args:body)) env c mem = c (LAMBDA args body) env mem
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
    if ob == [] then do
      S.liftIO $ putStrLn "Ошибка ввода"
      else do
      let (res, e', mem') = eval (fst $ last $ ob) e (\o e m -> (o, e, m)) mem
      S.liftIO $ putStrLn (show res)
      S.put (e', mem')
    process

repl :: IO ()
repl = do
  res <- S.runStateT process (globalEnv, initMem)
  return ()
main = repl
