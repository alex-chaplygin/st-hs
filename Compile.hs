import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

data SpecialConst = Receiver | True | False | Nil | MinusOne | Zero | One | Two
  deriving Show
data ArithMessage = ADD
  | SUB
  | MUL
  | DIV
  | LESS
  deriving Show
data Bytecode =
  PUSHLIT Integer -- literal num
  | PushInstanceVar Int
  | PushArg Int
  | PushSpecial SpecialConst
  | StoreInstanceVar Int
  | SendArith ArithMessage
  | Return -- top of stack
  | ReturnReceiver
  deriving Show

data Expr a =
  ENUM a
  | EID String
  | EMSG (Expr a) [(String, Expr a)]-- message: obj selector params
  | EADD (Expr a) (Expr a)
  | ESUB (Expr a) (Expr a)
  | EMUL (Expr a) (Expr a)
  | EDIV (Expr a) (Expr a)
  | ELESS (Expr a) (Expr a)
  deriving (Show)

data Operator a =
  ORET (Expr a)
  | OASS (Expr a) (Expr a)
  deriving Show

digit :: ReadP Int
digit = do
  c <- get
  if c >= '0' && c <= '9' then return $ ord c - ord '0' else pfail

number :: ReadP (Expr Integer)
number = do
  skipSpaces
  s <- munch1 (\c -> c >= '0' && c <= '9' || c == '-')
  return $ ENUM (read s)

mychar c = do {skipSpaces; char c}

plus = do {mychar '+'; return $ \x y -> EADD x y}
minus = do {mychar '-'; return $ \x y -> ESUB x y}
mul = do {mychar '*'; return $ \x y -> EMUL x y}
div' = do {mychar '/'; return $ \x y -> EDIV x y}
less = do {mychar '<'; return $ \x y -> ELESS x y}
ident = do {skipSpaces; c <- satisfy isAlpha; s <- munch isAlphaNum; return $ EID (c:s)}
keyword = do {(EID s) <- ident; char ':'; return $ s ++ ":"} -- max:
expr :: ReadP (Expr Integer)
pair = do {k <- keyword; e <- expr; return (k, e)}
message = do {(EID obj) <- ident; (EID sel) <- ident; params <- many pair; return $ EMSG obj sel params}
umessage = do {obj <- expr; (EID sel) <- ident; return $ EMSG obj sel []}
factor = number +++ umessage +++ ident +++ do {mychar '(' ; e <- expr ; mychar ')'; return e}
term = chainl1 factor $ choice [mul, div']
sumexpr = option (error "Error in expression") $ chainl1 term $ choice [plus, minus]
expr = chainl1 sumexpr $ choice [less]
opReturn = do {mychar '^'; e <- expr; mychar '.'; return $ ORET e}
opAssign = do
  l <- expr
  mychar '<'
  char '-'
  e <- expr
  mychar '.'
  return $ OASS l e
  
operator = option (error "Invalid operator") $ opReturn +++ opAssign

parse s = fst $ last $ readP_to_S operator s

--compileExpr :: Expr Integer -> [String] -> [String] -> [Bytecode]
compileExpr (ENUM 0) _ _ = [PushSpecial Zero]
compileExpr (ENUM 1) _ _ = [PushSpecial One]
compileExpr (ENUM (-1)) _ _ = [PushSpecial MinusOne]
compileExpr (ENUM 2) _ _ = [PushSpecial Two]
compileExpr (ENUM x) _ _ = [PUSHLIT x]
compileExpr (EID i) instVars args = case (elemIndex i instVars) of
  Just num -> [PushInstanceVar num]
  Nothing -> case (elemIndex i args) of
    Just num -> [PushArg num]
    _ -> error $ "Unknown var in expression: " ++ (show i)
compileExpr (EADD x y) i a = compileExpr x i a ++ compileExpr y i a ++ [SendArith ADD]
compileExpr (ESUB x y) i a = compileExpr x i a ++ compileExpr y i a ++ [SendArith SUB]
compileExpr (EMUL x y) i a = compileExpr x i a ++ compileExpr y i a ++ [SendArith MUL]
compileExpr (EDIV x y) i a = compileExpr x i a ++ compileExpr y i a ++ [SendArith DIV]
compileExpr (ELESS x y) i a = compileExpr x i a ++ compileExpr y i a ++ [SendArith LESS]

compile' (ORET e) i a = compileExpr e i a ++ [Return]
compile' (OASS e1 e2) instVars a = case e1 of
  EID var -> compileExpr e2 instVars a ++ case (elemIndex var instVars) of
    Just num -> [StoreInstanceVar num]
    _ -> error $ "Unknown instance var in assign: " ++ (show var)
  _ -> error $ "Not var on the left of assign"

compile meth inst args = let c = compile' meth inst args in
  case (last c) of
    Return -> c
    _ -> c ++ [ReturnReceiver]
    
assembleOp :: Bytecode -> Int
assembleOp (PushInstanceVar i) = i
assembleOp (PushArg i) = 16 + i
assembleOp (StoreInstanceVar i) = 96 + i
assembleOp (PushSpecial Two) = 119
assembleOp ReturnReceiver = 120
assembleOp Return = 124
assembleOp (SendArith ADD) = 176
assembleOp (SendArith LESS) = 178
assembleOp (SendArith DIV) = 185
assembleOp un = error $ "Unknown bytecode " ++ (show un)

assemble = map assembleOp
