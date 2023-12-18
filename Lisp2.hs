-- Интерпретатор Лисп с 2 окружениями для переменных и функций
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

type Environment = [(String, Object)]

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

t = SYMBOL "T"
nil = LIST []
globalEnv = [("T", t), ("NIL", nil)]

eq (LIST []: LIST []: []) = t
eq (SYMBOL a: SYMBOL b:_) = if a == b then t else nil
eq _ = nil

equal (NUM a: NUM b: _) = if a == b then t else nil
equal _ = nil

evalCar (LIST l) = head l
evalCar _ = error "not list in CAR"

evalCdr (LIST l) = LIST $ tail l
evalCdr _ = error "not list in CDR"

cons (a:[LIST b]) = LIST (a:b)
cons _ = error "Invalid CONS"

eval :: Environment -> Environment -> Object -> (Object, Environment, Environment)
cond _ _ [] = error "empty cond"
cond env fenv ((LIST (p:e)):tail) = let (p', _, _) = eval env fenv p in
  if p' == t then eval env fenv $ head e
  else cond env fenv tail

-- поиск символа в окружении
envLookup :: String -> Environment -> Object
envLookup var [] = error $ "Нет переменной " ++ (show var)
envLookup var ((key, obj):tail) = if key == var then obj else envLookup var tail
-- обновление переменной в окружении
update :: String -> Object -> Environment -> Environment
update var val env = update' var val env [] where
  update' var val [] env = (var, val):env
  update' var val ((key, obj):tail) env = if key == var then (key, val):tail else
    (key, obj):update var val tail

defun env (SYMBOL name:params:body) =
  let lam = LAMBDA params body env in update name lam env

evalExpr f (h:tail) = foldl f (unInt h) $ map unInt tail
  where unInt (NUM i) = i
        unInt _ = error "Не число в выражении"
-- создать окружение для функции (переменная, значение)
makeEnv :: Object -> [Object] -> Environment
makeEnv (LIST params) values = if length params /= length values then
  error "Invalid params count"
  else zip (map fromAtom params) values
  where fromAtom (SYMBOL a) = a
        fromAtom _ = error "Not atoms in params"
makeEnv _ _ = error "No params list"             

-- применение функции
-- Окружение переменных->Окружение функций-> Имя функции -> Аргументы -> (Результат, Новое окружение переменных, Окружение функций)
apply :: Environment -> Environment -> Object -> [Object] -> (Object, Environment, Environment)
apply env fenv (LAMBDA params body funEnv) args = 
  let env' = (makeEnv params args) ++ funEnv ++ env in -- вычислить кадр стека окружения
    eval env' fenv $ LIST (SYMBOL "PROGN":body) -- невный PROGN
apply _ _ _ _ = error "Применяется не функция"
-- вычисление списка аргументов
calcArgs :: Environment -> Environment -> [Object] -> [Object]
calcArgs env fenv args = map (\(a, _, _)->a) $ map (eval env fenv) args

eval e f (NUM i) = (NUM i, e, f)
eval e f (LIST []) = (LIST [], e, f)
eval env fenv (SYMBOL var) = (envLookup var env, env, fenv)
-- особые формы     
eval e f (LIST (SYMBOL "QUOTE":cdr)) = (head cdr, e, f)
eval env fenv (LIST (SYMBOL "COND":cdr)) = cond env fenv cdr
eval env fenv (LIST (SYMBOL "PROGN":cdr)) = foldl (\(o, e, f) obj->eval e f obj) (nil, env, fenv) cdr
eval env fenv (LIST (SYMBOL "SETQ":SYMBOL var:expr)) =
  let (v, _) = eval env fenv (head expr) in (v, update var v env, fenv)
eval env fenv (LIST (SYMBOL "LAMBDA":args:body)) = (LAMBDA args body env, env, fenv)
eval env fenv (LIST (LIST (SYMBOL "LAMBDA":args:body)):vals) = apply env fenv (LAMBDA args body env) $ calcArgs env fenv vals
eval env fenv (LIST (car:cdr)) =
  let args = calcArgs env fenv cdr in
  case car of -- встроенные примитивы
   SYMBOL "+" -> (NUM $ evalExpr (+) args, env, fenv)
   SYMBOL "-" -> (NUM $ evalExpr (-) args, env, fenv)
   SYMBOL "*" -> (NUM $ evalExpr (*) args, env, fenv)
   SYMBOL "/" -> (NUM $ evalExpr div args, env, fenv)
   SYMBOL "EQ" -> (eq args, env, fenv)
   SYMBOL "EQUAL" -> (equal args, env, fenv)
   SYMBOL "CAR" -> (evalCar $ head args, env, fenv)
   SYMBOL "CDR" -> (evalCdr $ head args, env, fenv)
   SYMBOL "CONS" -> (cons args, env, fenv)
   SYMBOL "LIST" -> (LIST args, env, fenv)
   SYMBOL "DEFUN" -> (head cdr, env, defun fenv cdr)
   SYMBOL "FUNCALL" -> (apply env fenv (head args) (tail args), env, fenv)
   SYMBOL f -> apply env fenv (envLookup f fenv) args
   _ -> error "Неправильный вызов функции"

process :: S.StateT (Environment, Environment) IO ()
process = do
  (e, f) <- S.get -- получаем текущие окружения
  S.liftIO $ putStr "> "
  str <- S.liftIO $ getLine
  if str == "q" then return ()
    else do
    let ob = parse str
    if ob == [] then do
      S.liftIO $ putStrLn "Ошибка ввода"
      else do
      let (res, env, fenv) = eval e f (fst $ last $ ob)
      S.liftIO $ putStrLn (show res)
      S.put (env, fenv)
    process

repl :: IO ()
repl = do
  let env = globalEnv
      fenv = []
  res <- S.runStateT process (env, fenv)
  return ()
main = repl
