import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

data Object = ATOM String
  | NUM Int
  | LIST [Object]
  deriving (Show, Eq)

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

eval :: Object -> Object
cond [] = error "empty cond"
cond ((LIST (p:e)):t) = let p' = eval p in
  if p' == ATOM "T" then eval $ head e
  else cond t

-- создать окружение для функции (переменная, значение)
makeEnv :: Object -> [Object] -> [(String, Object)]
makeEnv (LIST params) values = zip (map fromAtom params) values
  where fromAtom (ATOM a) = a
        fromAtom _ = error "Not atoms in params"

lambda :: Object -> Object -> [Object] -> Object
lambda params body args = params
--  let pars = makeEnv params $ map eval args in
--    eval $ fmap f body
--    where f (ATOM s) = 

eval (NUM i) = NUM i
eval (LIST []) = LIST []
eval (LIST (ATOM "QUOTE":cdr)) = head cdr
eval (LIST (ATOM "COND":cdr)) = cond cdr
eval (LIST (LIST (ATOM "LAMBDA":params:body)):args) = lambda params body args
eval (car:cdr) =
    let args = map eval cdr in
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
eval s = error $ "Error: " ++ (show s)
