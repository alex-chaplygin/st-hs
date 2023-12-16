import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Monad.State as S

data Object = SYMBOL String
  | NUM Int
  | LIST [Object]
  deriving (Eq)

type Environment = Map String Object

data Result = OK Object
  | ERROR String
  deriving Eq

instance Show Object where
  show (SYMBOL s) = s
  show (NUM i) = (show i)
  show (LIST []) = "NIL"
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
symbol = do {skipSpaces; c <- satisfy atomFirst; s <- munch atomSym; return $ SYMBOL $ map toUpper (c:s)}
atom = symbol +++ number
obj = atom +++ qexpr
sexpr =  atom +++ do {mychar '(' ; e <- many obj; mychar ')'; return $ LIST e}
qexpr = do {mychar '\''; e <- sexpr; return $ case e of
               LIST l -> LIST $ (SYMBOL "QUOTE") : [LIST l]
               NUM n -> LIST $ (SYMBOL "QUOTE") : [NUM n]
               b -> LIST $ (SYMBOL "QUOTE") : [b]
           } +++ sexpr
parse s = readP_to_S obj s

t = SYMBOL "T"
nil = LIST []
evalAtom (LIST []) = t
evalAtom (SYMBOL _) = t
evalAtom _ = nil

eq (SYMBOL a: SYMBOL b:_) = if a == b then t else nil
eq _ = nil

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

defun env (SYMBOL name:params:body) =
  let lam = LIST $ (SYMBOL "LAMBDA"):params:body in
  Map.insert name lam env

funcall env f args = case Map.lookup f env of
     Nothing -> error $ "Unknown function " ++ (show f)
     Just val -> eval env $ LIST $ val:args

evalExpr f (h:tail) = foldl f (unInt h) $ map unInt tail
  where unInt (NUM i) = i
        unInt _ = error "Не число в выражении"

-- создать окружение для функции (переменная, значение)
makeEnv :: Object -> [Object] -> Environment
makeEnv (LIST params) values = if length params /= length values then
  error "Invalid params count"
  else
  Map.fromList $ zip (map fromAtom params) values
  where fromAtom (SYMBOL a) = a
        fromAtom _ = error "Not atoms in params"
makeEnv _ _ = error "No params list"             

eval e (NUM i) = (NUM i, e)
eval e (LIST []) = (LIST [], e)
eval e (SYMBOL "T") = (t, e)
eval e (SYMBOL "NIL") = (nil, e)
eval env (SYMBOL var) = case Map.lookup var env of
     Nothing -> error $ "Unknown variable " ++ (show var)
     Just val -> (val, env)
eval e (LIST (SYMBOL "QUOTE":cdr)) = (head cdr, e)
eval env (LIST (SYMBOL "COND":cdr)) = cond env cdr
eval env (LIST (SYMBOL "PROGN":cdr)) = foldl (\(o, e) obj->eval e obj) (nil, env) cdr
--eval env (LIST ((LIST (SYMBOL "LAMBDA":params:body)):args)) =
--  let args' = map fst $ map (eval env) args in -- вычислить аргументы
--  let env' = Map.union (makeEnv params args') env in -- вычислить новое окружение
--  eval env' $ head body
eval env (LIST (car:cdr)) =
  let args = map fst $ map (eval env) cdr in
  case car of
   SYMBOL "+" -> (NUM $ evalExpr (+) args, env)
   SYMBOL "-" -> (NUM $ evalExpr (-) args, env)
   SYMBOL "*" -> (NUM $ evalExpr (*) args, env)
   SYMBOL "/" -> (NUM $ evalExpr div args, env)
   SYMBOL "SYMBOL" -> (evalAtom $ head args, env)
   SYMBOL "EQ" -> (eq args, env)
   SYMBOL "CAR" -> (evalCar $ head args, env)
   SYMBOL "CDR" -> (evalCdr $ head args, env)
   SYMBOL "CONS" -> (cons args, env)
   SYMBOL "DEFUN" -> (SYMBOL "T", defun env cdr)
   SYMBOL f -> funcall env f cdr
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
    OK (SYMBOL "T") -> ev $ head e
    OK _ -> cond' t
    ERROR s -> return $ ERROR s

--lambda' params body args =
--  let args' = map unOk $ map ev args in -- вычислить аргументы
--  let env' = Map.union (makeEnv params args') env in -- вычислить новое окружение
--  eval env' $ head body

ev (NUM i) = return $ OK $ NUM i
ev (SYMBOL var) = lookVar var
ev (LIST []) = return $ OK $ LIST []
ev (LIST (SYMBOL "QUOTE":cdr)) = return $ OK $ head cdr
ev (LIST (SYMBOL "COND":cdr)) = cond' cdr
--ev (LIST ((LIST (SYMBOL "LAMBDA":params:body)):args)) = lambda' params body args
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
