import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Monad.State as S

data Object = ATOM String
  | NUM Int
  | LIST [Object]
  deriving (Show, Eq)

type Environment = Map String Object

isNotDigit c = c < '0' || c > '9'

number :: ReadP Object
number = do
  skipSpaces
  s <- munch1 isDigit
  return $ NUM (read s)

mychar c = do {skipSpaces; char c}

atomFirst c = (isAlpha c || isSymbol c) && c /= '(' && c /= ')'
atomSym c = (isAlphaNum c || isSymbol c) && c /= '(' && c /= ')'

qexpr :: ReadP Object
atom = do {skipSpaces; c <- satisfy atomFirst; s <- munch atomSym; return $ ATOM $ map toUpper (c:s)}
obj = atom +++ number +++ qexpr

sexpr =  atom +++ do {mychar '(' ; e <- many obj; mychar ')'; return $ LIST e}
qexpr = do {mychar '\''; e <- sexpr; return $ case e of
               LIST l -> LIST $ (ATOM "QUOTE") : [LIST l]
               b -> LIST $ (ATOM "QUOTE") : [b]
           } +++ sexpr
parse s = fst $ last $ readP_to_S qexpr s

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

eval :: Environment -> Object -> Object
cond _ [] = error "empty cond"
cond env ((LIST (p:e)):t) = let p' = eval env p in
  if p' == ATOM "T" then eval env $ head e
  else cond env t

-- создать окружение для функции (переменная, значение)
makeEnv :: Object -> [Object] -> Environment
makeEnv (LIST params) values = if length params /= length values then
  error "Invalid params count"
  else
  Map.fromList $ zip (map fromAtom params) values
  where fromAtom (ATOM a) = a
        fromAtom _ = error "Not atoms in params"
makeEnv _ _ = error "No params list"             
                                               
eval _ (NUM i) = NUM i
eval env (ATOM var) = case Map.lookup var env of
     Nothing -> error $ "Unknown variable " ++ (show var)
     Just val -> val
eval _ (LIST []) = LIST []
eval _ (LIST (ATOM "QUOTE":cdr)) = head cdr
eval env (LIST (ATOM "COND":cdr)) = cond env cdr
eval env (LIST ((LIST (ATOM "LAMBDA":params:body)):args)) =
  let args' = map (eval env) args in -- вычислить аргументы
  let env' = Map.union (makeEnv params args') env in -- вычислить новое окружение
  eval env' $ head body
eval env (LIST (car:cdr)) =
  let args = map (eval env) cdr in
  case car of
--   ATOM "+" -> INT $ foldl (+) h args
--   ATOM "-" -> INT $ foldl (-) h args
--   ATOM "*" -> INT $ foldl (*) h args
--   ATOM "/" -> INT $ foldl div h args
   ATOM "ATOM" -> evalAtom $ head args
   ATOM "EQ" -> eq args
   ATOM "CAR" -> evalCar $ head args
   ATOM "CDR" -> evalCdr $ head args
   ATOM "CONS" -> cons args
   _ -> error "Unknown function"

process :: S.StateT Environment IO ()
process = do
  env <- S.get -- получаем текущее окружение
  S.liftIO $ putStr "> "
  str <- S.liftIO $ getLine
  if str == "q" then return ()
    else do
    S.liftIO $ putStrLn $ show $ eval env $ parse str
    process

repl :: IO ()
repl = do
  let env = Map.empty
  res <- S.runStateT process env
  return ()
