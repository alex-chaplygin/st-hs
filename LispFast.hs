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
data FrameList = FrameList -- записи активаций
  { nextFrame :: Env -- следующий кадр
  , frameEnv :: Frame -- значения аргументов при вызове функций
  }
type Cont = Object -> Env -> (Object, Env)-- продолжение

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
-- анализатор программы
meaning :: Object -> Env -> FrameList -> Cont -> Object
meaning o@(NUM i) e = meaning-quote o e
meaning (LIST (SYMBOL "QUOTE":o:[])) e = meaning-quote o e
meaning (SYMBOL name) e = meaning-reference name e
meaning (LIST (SYMBOL "LAMBDA":args:body)) e = meaning-abstraction args body e
meaning (LIST (SYMBOL "IF":expr:t:f:[])) e = meaning-alternative expr t f e
meaning (LIST (SYMBOL "BEGIN":s)) e = meaning-sequence s e
meaning (LIST (SYMBOL "SETQ":SYMBOL var:exp:[])) e = meaning-assignment var exp e
meaning (LIST (car:cdr)) e = meaning-application car cdr e
-- цитирование
meaning-quote o _ = \_ c -> c o
-- ветвление
meaning-alternative :: Object -> Env -> FrameList -> Cont -> Object
meaning-alternative e1 e2 e3 env = let m1 = meaning e1 env
                                       m2 = meaning e2 env
                                       m3 = meaning e3 env in
                                     \sr c -> m1 sr $ \v -> (case v of
                                       SYMBOL "T" -> m2
                                       LIST [] -> m3
                                       _ -> error "IF") $ sr c
-- последовательность
meaning-sequence (o:[]) e = meaning o e
meaning-sequence (o:t) e = let m1 = meaning o e
                               mt = meaning-sequence t e in
                             \sr c -> m1 sr $ \v -> mt sr c
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
