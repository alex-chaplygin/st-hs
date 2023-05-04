import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

data Object = ATOM String
  | NUM Int
  | NIL
  | CONS Object Object
  deriving (Show, Eq)

data Result = INT Int
  | OBJ Object
  deriving Show
-- (1 2)
-- CONS (NUM 1) (CONS (NUM 2) NIL)

isNotDigit c = c < '0' || c > '9'

number :: ReadP Object
number = do
  skipSpaces
  s <- munch1 isDigit
  return $ NUM (read s)

mychar c = do {skipSpaces; char c}

expr :: ReadP Object
atom = do {skipSpaces; c <- satisfy isNotDigit; s <- munch (\c -> c /= ' ' && c /= ')'); return $ ATOM (c:s)}
obj = atom +++ number +++ expr

toCons :: [Object] -> Object
toCons [] = NIL
toCons (h:t) = CONS h (toCons t)

expr =  do {mychar '(' ; e <- many obj; mychar ')'; return $ toCons e}
parse s = fst $ last $ readP_to_S expr s

makeArgs :: Object -> [Int]
eval :: Object -> Result
eval (NUM i) = INT i
eval (CONS car cdr) =
  let (h:args) = makeArgs cdr in
  case car of
   ATOM "+" -> INT $ foldl (+) h args
   ATOM "-" -> INT $ foldl (-) h args
   ATOM "*" -> INT $ foldl (*) h args
   ATOM "/" -> INT $ foldl div h args
   ATOM "QUOTE" -> let (CONS a b) = cdr in OBJ a
   ATOM "EVAL" -> eval cdr
   _ -> error "Not + or -"
eval s = error $ "Error: " ++ (show s)

makeArgs NIL = []               
makeArgs (CONS car cdr) =
  let v = eval car in
    case v of
      INT i -> i : makeArgs cdr
      _ -> error "Not Int"
               

