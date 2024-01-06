-- Базовый интерпретатор Лисп с одним окружением
import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import System.IO as Sys
import Data.Map (Map)
import qualified Control.Monad.State as S

data Object = SYMBOL String
  | NUM Int
  | LIST [Object]
  | LAMBDA Object [Object] Environment -- функция параметры тело окружение
  deriving (Eq)

data Op = CONST Object
  deriving (Show)

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
program = do {s <- many obj; return s}
parse s = readP_to_S program s

compileExpr pr n@(NUM _) = pr ++ [CONST n]
compileExpr pr (LIST (SYMBOL "QUOTE":q:[])) = pr ++ [CONST q]
compileExpr _ _ = error "compileExpr"

compile = foldl compileExpr []

showProg (o:[]) = show o
showProg (o:t) = show o ++ "\n" ++ showProg t

main :: IO ()
main = do
  s <- hGetContents Sys.stdin
  let prog = compile . fst . last . parse $ s
  putStrLn "Программа:"
  putStrLn . showProg $ prog
  putStrLn "Результат:"
  putStrLn . show . vm $ prog
