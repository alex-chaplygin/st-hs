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

eval :: Environment -> Object -> (Object, Environment)

cond _ [] = error "empty cond"
cond env ((LIST (p:e)):tail) = let (p', _) = eval env p in
  if p' == t then eval env $ head e
  else cond env tail

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
--       Окружение -> Функция -> Аргументы -> (Результат, Новое окружение)
apply :: Environment -> Object -> [Object] -> (Object, Environment)
apply env (LAMBDA params body fenv) args = 
  let env' = (makeEnv params args) ++ fenv ++ env in -- вычислить кадр стека окружения
    eval env' $ LIST (SYMBOL "PROGN":body) -- невный PROGN
apply _ _ _ = error "Применяется не функция"

eval e (NUM i) = (NUM i, e)
eval e (LIST []) = (LIST [], e)
eval env (SYMBOL var) = (envLookup var env, env)
-- особые формы     
eval e (LIST (SYMBOL "QUOTE":cdr)) = (head cdr, e)
eval env (LIST (SYMBOL "COND":cdr)) = cond env cdr
eval env (LIST (SYMBOL "PROGN":cdr)) = foldl (\(o, e) obj->eval e obj) (nil, env) cdr
eval env (LIST (SYMBOL "SETQ":SYMBOL var:expr)) =
  let (v, _) = eval env (head expr) in (v, update var v env)
eval env (LIST (SYMBOL "LAMBDA":args:body)) = (LAMBDA args body env, env)
eval env (LIST (car:cdr)) =
  let args = map fst $ map (eval env) cdr in
  case car of -- встроенные примитивы
   SYMBOL "+" -> (NUM $ evalExpr (+) args, env)
   SYMBOL "-" -> (NUM $ evalExpr (-) args, env)
   SYMBOL "*" -> (NUM $ evalExpr (*) args, env)
   SYMBOL "/" -> (NUM $ evalExpr div args, env)
   SYMBOL "EQ" -> (eq args, env)
   SYMBOL "EQUAL" -> (equal args, env)
   SYMBOL "CAR" -> (evalCar $ head args, env)
   SYMBOL "CDR" -> (evalCdr $ head args, env)
   SYMBOL "CONS" -> (cons args, env)
   SYMBOL "LIST" -> (LIST args, env)
   SYMBOL "DEFUN" -> (head cdr, defun env cdr)
   f -> let (func, env') = eval env f in
     apply env' func args

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
