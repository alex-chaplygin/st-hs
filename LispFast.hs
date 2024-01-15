-- Базовый интерпретатор Лисп с одним окружением
import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Vector (Vector, (!), (//), empty, cons)
import Data.Hashable
import qualified Control.Monad.State as S

data Object = SYMBOL String
  | NUM Int
  | LIST [Object]
  | LAMBDA Object [Object] -- функция параметры тело окружение
  deriving (Eq)

type Env = Vector Object -- массив значений (запись активации)
type FrameList = [Env]
type Cont = Object -> (Object, Env)-- продолжение

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
--globalEnv :: Env
--globalEnv = "T" = 1
--globalEnv "NIL" = 2
--globalEnv var = error $"Нет переменной " ++ (show var)
-- анализатор программы
meaning :: Object -> Env -> FrameList -> Cont -> (Object, Env)
meaning o@(NUM i) e = meaningQuote o e
meaning (LIST (SYMBOL "QUOTE":o:[])) e = meaningQuote o e
-- meaning (SYMBOL name) e = meaningReference name e
--meaning (LIST (SYMBOL "LAMBDA":args:body)) e = meaningAbstraction args body e
meaning (LIST (SYMBOL "IF":expr:t:f:[])) e = meaningAlternative expr t f e
meaning (LIST (SYMBOL "BEGIN":s)) e = meaningSequence s e
--meaning (LIST (SYMBOL "SETQ":SYMBOL var:exp:[])) e = meaning-assignment var exp e
--meaning (LIST (car:cdr)) e = meaningApplication car cdr e
-- цитирование
meaningQuote o e = \sr c -> c o
-- ветвление
meaningAlternative e1 e2 e3 env = let m1 = meaning e1 env
                                      m2 = meaning e2 env
                                      m3 = meaning e3 env in
                                    \sr c -> m1 sr $ \v -> (case v of
                                        SYMBOL "T" -> m2
                                        LIST [] -> m3
                                        _ -> error "IF") sr c
-- последовательность
meaningSequence (o:[]) e = meaning o e
meaningSequence (o:t) e = let m1 = meaning o e
                              mt = meaningSequence t e in
                             \sr c -> m1 sr $ \v -> mt sr c
-- применение функции
-- meaningApplication f args env = let m = meaning f env
--                                     vals = meaningArgs args env $ length args in
--                                   \sr c -> m sr $ \f' -> vals sr $ \v' -> f v' c
-- -- вычисление аргументов
-- meaningArgs [] _ _ = \sr c -> c empty
-- meaningArgs (car:cdr) env size = let m = meaning car env
--                                      mt = meaningArgs cdr env
--                                      idx = size - length cdr - 1 in
--                                    \sr c -> m sr $ \v -> mt sr $ \v' -> set-frame
-- -- абстракция
-- meaningAbstraction (LIST args) body env = let arity = length args
--                                               arity' = arity + 1
--                                               env' = cons args env
--                                               m = meaningSequence body env' in
--                                             \sr c -> c $ \o c' -> if length o == arity' then m (o : sr) c' else error "Неверное число аргументов"

-- глубокий поиск переменной - номер кадра - номер записи
deepFetch :: FrameList -> Int -> Int -> Object
deepFetch (car:cdr) 0 j = car ! j
deepFetch (car:cdr) i j = deepFetch cdr (i - 1) j
-- глубокое обновление переменной
deepUpdate :: FrameList -> Int -> Int -> Object -> FrameList
deepUpdate (car:cdr) 0 j v = (car // [(j, v)]) : cdr
deepUpdate (car:cdr) i j v = deepUpdate cdr (i-1) j v

process :: S.StateT Env IO ()
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
      let (res, e') = meaning (fst $ last $ ob) e [] $ \o -> (o, e)
      S.liftIO $ putStrLn (show res)
      S.put e'
    process

repl :: IO ()
repl = do
  res <- S.runStateT process empty
  return ()
main = repl
