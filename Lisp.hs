import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Monad.State as S

data Object = ATOM String
  | NUM Int
  | LIST [Object]
  deriving (Eq)

type Environment = Map String Object

data Result = OK Object
  | ERROR String
  deriving Eq

instance Show Object where
  show (ATOM s) = s
  show (NUM i) = (show i)
  show (LIST l) = "(" ++ (concat $ intersperse " " $ map show l) ++ ")"

isNotDigit c = c < '0' || c > '9'
isMySymbol c = isSymbol c || c == '*' || c == '/'

number :: ReadP Object
number = do
  skipSpaces
  s <- munch1 isDigit
  return $ NUM (read s)

mychar c = do {skipSpaces; char c}

atomFirst c = (isAlpha c || isMySymbol c) && c /= '(' && c /= ')'
atomSym c = (isAlphaNum c || isMySymbol c) && c /= '(' && c /= ')'

qexpr :: ReadP Object
atom = do {skipSpaces; c <- satisfy atomFirst; s <- munch atomSym; return $ ATOM $ map toUpper (c:s)}
obj = atom +++ number +++ qexpr

sexpr =  atom +++ do {mychar '(' ; e <- many obj; mychar ')'; return $ LIST e}
qexpr = do {mychar '\''; e <- sexpr; return $ case e of
               LIST l -> LIST $ (ATOM "QUOTE") : [LIST l]
               b -> LIST $ (ATOM "QUOTE") : [b]
           } +++ sexpr
parse s = readP_to_S qexpr s

evalAtom (LIST []) = ATOM "T"
evalAtom (ATOM _) = ATOM "T"
evalAtom _ = LIST []

eq (ATOM a: ATOM b:_) = if a == b then ATOM "T" else LIST []
eq (LIST a: LIST b:_) = if a == b then ATOM "T" else LIST []
eq _ = LIST []

evalCar (LIST l) = head l
evalCar _ = error "not list in CAR"

evalCdr (LIST l) = LIST $ tail l
evalCdr _ = error "not list in CDR"

cons (a:[LIST b]) = LIST (a:b)
cons _ = error "Invalid CONS"

eval :: Environment -> Object -> (Object, Environment)
cond _ [] = error "empty cond"
cond env ((LIST (p:e)):t) = let (p', _) = eval env p in
  if p' == ATOM "T" then eval env $ head e
  else cond env t

defun env (ATOM name:params:body) =
  let lam = LIST $ (ATOM "LAMBDA"):params:body in
  Map.insert name lam env

funcall env f args = case Map.lookup f env of
     Nothing -> error $ "Unknown function " ++ (show f)
     Just val -> eval env $ LIST $ val:args

evalExpr f (h:t) = foldl f (unInt h) $ map unInt t
  where unInt (NUM i) = i
        unInt _ = error "Не число в выражении"

-- создать окружение для функции (переменная, значение)
makeEnv :: Object -> [Object] -> Environment
makeEnv (LIST params) values = if length params /= length values then
  error "Invalid params count"
  else
  Map.fromList $ zip (map fromAtom params) values
  where fromAtom (ATOM a) = a
        fromAtom _ = error "Not atoms in params"
makeEnv _ _ = error "No params list"             

eval e (NUM i) = (NUM i, e)
eval env (ATOM var) = case Map.lookup var env of
     Nothing -> error $ "Unknown variable " ++ (show var)
     Just val -> (val, env)
eval e (LIST []) = (LIST [], e)
eval e (LIST (ATOM "QUOTE":cdr)) = (head cdr, e)
eval env (LIST (ATOM "COND":cdr)) = cond env cdr
eval env (LIST ((LIST (ATOM "LAMBDA":params:body)):args)) =
  let args' = map fst $ map (eval env) args in -- вычислить аргументы
  let env' = Map.union (makeEnv params args') env in -- вычислить новое окружение
  eval env' $ head body
eval env (LIST (car:cdr)) =
  let args = map fst $ map (eval env) cdr in
  case car of
   ATOM "+" -> (NUM $ evalExpr (+) args, env)
   ATOM "-" -> (NUM $ evalExpr (-) args, env)
   ATOM "*" -> (NUM $ evalExpr (*) args, env)
   ATOM "/" -> (NUM $ evalExpr div args, env)
   ATOM "ATOM" -> (evalAtom $ head args, env)
   ATOM "EQ" -> (eq args, env)
   ATOM "CAR" -> (evalCar $ head args, env)
   ATOM "CDR" -> (evalCdr $ head args, env)
   ATOM "CONS" -> (cons args, env)
   ATOM "DEFUN" -> (ATOM "T", defun env cdr)
   ATOM f -> funcall env f cdr
   _ -> error "Не функция"

lookVar :: String -> S.State Environment Result
lookVar var = do
  env <- S.get
  return $ case Map.lookup var env of
     Nothing -> ERROR $ "Неизвестная переменная " ++ (show var)
     Just val -> OK val

ev :: Object -> S.State Environment Result

cond' [] = return $ ERROR "Пустое условие COND"
cond' ((LIST (p:e)):t) = do
  env <- S.get
  let (p', _) = S.runState (ev p) env
  case p' of
    OK (ATOM "T") -> ev $ head e
    OK _ -> cond' t
    ERROR s -> return $ ERROR s

--lambda' params body args =
--  let args' = map unOk $ map ev args in -- вычислить аргументы
--  let env' = Map.union (makeEnv params args') env in -- вычислить новое окружение
--  eval env' $ head body

ev (NUM i) = return $ OK $ NUM i
ev (ATOM var) = lookVar var
ev (LIST []) = return $ OK $ LIST []
ev (LIST (ATOM "QUOTE":cdr)) = return $ OK $ head cdr
ev (LIST (ATOM "COND":cdr)) = cond' cdr
--ev (LIST ((LIST (ATOM "LAMBDA":params:body)):args)) = lambda' params body args
ev _ = return $ ERROR "ER"

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
      S.liftIO $ putStrLn (show res) --of
--        OK o -> (show o)
--        ERROR e -> "Ошибка: " ++ e
      S.put env
    process

repl :: IO ()
repl = do
  let env = Map.empty
  res <- S.runStateT process env
  return ()
