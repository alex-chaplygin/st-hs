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

eq (ATOM a: ATOM b:_) = if a == b then ATOM "T" else LIST []
eq (LIST a: LIST b:_) = if a == b then ATOM "T" else LIST []
eq _ = LIST []

evalCar (LIST l) = head l
evalCar _ = error "not list in CAR"

evalCdr (LIST l) = LIST $ tail l
evalCdr _ = error "not list in CDR"

--makeArgs :: [Object] -> [Int]
eval :: Object -> Object
eval (NUM i) = NUM i
eval (LIST []) = LIST []
eval (LIST (car:cdr)) =
--  let (h:args) = makeArgs cdr in
  case car of
--   ATOM "+" -> INT $ foldl (+) h args
--   ATOM "-" -> INT $ foldl (-) h args
--   ATOM "*" -> INT $ foldl (*) h args
--   ATOM "/" -> INT $ foldl div h args
   ATOM "QUOTE" -> head cdr
   ATOM "ATOM" -> case (eval $ head cdr) of
                    LIST [] -> ATOM "T"
                    ATOM _ -> ATOM "T"
                    _ -> LIST []
   ATOM "EQ" -> eq cdr
   ATOM "CAR" -> evalCar $ eval $ head cdr
   ATOM "CDR" -> evalCdr $ eval $ head cdr
   _ -> error "Unknown function"
eval s = error $ "Error: " ++ (show s)

--makeArgs [] = []               
--makeArgs (car:cdr) =
--  let v = eval car in
--    case v of
--      INT i -> i : makeArgs cdr
--      _ -> error "Not Int"
               

