-- Интерпретатор Лисп с продолжениями
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

                  -- имя значение остальные 
type Environment = [(String, Object)]
                 --     продолжение выражИстина выражЛожь окружение
data Continuation = IfContinuation Continuation Object Object Environment
                  | BeginContinuation Continuation [Object] Environment
                  | SetContinuation Continuation String Environment
                  -- вычисление функции
                  | FunContinuation Continuation [Object] Environment
                  -- 
                  | ApplyContinuation Continuation Object Environment

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

eval :: Object -> Environment -> Continuation -> (Object, Environment)
evalBegin :: [Object] -> Environment -> Continuation -> (Object, Environment)

-- обновление переменной в окружении
update :: Environment -> String -> Continuation -> Object -> (Object, Environment)
update env var val = update' var val env [] where
  update' var val [] env = (var, val):env
  update' var val ((key, obj):tail) env = if key == var then (key, val):tail else
    (key, obj):update var val tail
-- обновление окружения внутри продолжения
updateCont :: Continuation -> String -> Object -> Continuation
updateCont (IfContinuation cc o1 o2 env) var val = IfContinuation cc o1 o2 $ update env var val
updateCont (BeginContinuation cc o env) var val = BeginContinuation cc o $ update env var val
updateCont (SetContinuation cc s env) var val = BeginContinuation cc s $ update env var val
updateCont (FunContinuation cc s env) var val = FunContinuation cc s $ update env var val

-- возобновление сохраненного продолжения
resume :: Continuation -> Object -> (Object, Environment)
resume (IfContinuation cc true _ env) (SYMBOL "T") = eval true env cc
resume (IfContinuation cc _ false env) (LIST []) = eval false env cc
resume (BeginContinuation cc (_:cdr) env) _ = evalBegin cdr env cc
resume (SetContinuation _ var []) _ = error $ "Unknown variable" ++ $ show var
resume (SetContinuation cc var env) val = resume (updateCont cc var val) val
resume (FunContinuation cc args env) f = resume 

evalQuote :: Object -> Environment -> Continuation -> (Object, Environment)
evalQuote obj env cc = resume cc obj

evalIf :: Object -> Object -> Object -> Environment -> Continuation -> (Object, Environment)
evalIf expr true false env cc = eval expr env $ IFCONT cc true false env

evalBegin :: [Object] -> Environment -> Continuation -> (Object, Environment)
evalBegin [] env cc = resume cc $ LIST []
evalBegin (car:[]) env cc = eval car env cc
evalBegin (car:cdr) env cc = eval car env $ BeginContinuation cc (car:cdr) env

evalVar :: String -> Environment -> Continuation -> (Object, Environment)
evalVar n [] _ = error $ "Unknown var" ++ (show n)
evalVar n ((var, val):cdr) cc = if n == var then resume cc val
  else evalVar n cdr cc

evalSet :: String -> Object -> Environment -> Continuation -> (Object, Environment)
evalSet var expr env cc = eval expr env $ SetContinuation cc var env
-- Ламбда    аргументы
evalLambda :: Object -> [Object] -> Environment -> Continuation -> (Object, Environment)
evalLambda args body env cc = resume cc $ LAMBDA args body env

-- создать окружение для функции (переменная, значение)
makeEnv :: Object -> [Object] -> Environment
makeEnv (LIST params) values = if length params /= length values then
  error "Invalid params count"
  else zip (map fromAtom params) values
  where fromAtom (SYMBOL a) = a
        fromAtom _ = error "Not atoms in params"
makeEnv _ _ = error "No params list"             

-- вызов функции ламбда->аргументы->окружение->продолжение
invoke :: Object -> [Object] -> Environment -> Continuation -> (Object, Environment)
invoke (LAMBDA args body e) vals env cc = let newEnv = makeEnv args vals ++ e in
  evalBegin body newEnv cc

evalApp :: Object -> [Object] -> Environment -> Continuation -> (Object, Environment)
evalApp f args env cc = eval f env $ FunContinuation cc args env

eval (NUM i) env cc = NUM i
eval (LIST []) env cc = LIST []
eval (SYMBOL var) env cc = evalVar var env cc
-- особые формы     
eval (LIST (SYMBOL "QUOTE":cdr)) env cc = evalQuote (head cdr) env cc
eval (LIST (SYMBOL "IF":expr:true:false)) env cc = evalIf expr true (head false) env cc
eval (LIST (SYMBOL "BEGIN":cdr)) env cc = evalBegin cdr env cc
eval (LIST (SYMBOL "SETQ":SYMBOL var:expr)) env cc = evalSet var (head expr) env cc
eval (LIST (SYMBOL "LAMBDA":args:body)) env cc = evalLambda args body env cc
eval (LIST (car:cdr)) env cc = evalApp car cdr env cc

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
