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
  | PRIM String
  | CONT Continuation
  deriving (Eq)

                  -- имя значение остальные 
type Environment = [(String, Object)]
                 --     продолжение выражИстина выражЛожь окружение
data Continuation = IfContinuation Continuation Object Object Environment
                  | BeginContinuation Continuation [Object] Environment
                  | SetContinuation Continuation String Environment
                  -- вычисление функции 
                  | FunContinuation Continuation [Object] Environment
                  -- применение функции после вычисления аргументов
                  | ApplyContinuation Continuation Object Environment
                  -- вычисление аргумента
                  | ArgContinuation Continuation [Object] Environment
                  -- сохранение вычисленного аргумента
                  | GathContinuation Continuation Object
                  -- установка метки                тело 
                  | CatchContinuation Continuation [Object] Environment
                  -- после вычисления тела          тег
                  | LabelContinuation Continuation Object
                  -- после выброса исключения       форма
                  | ThrowContinuation Continuation Object Environment
                  -- после вычисления значения исключения
                  | ThrowingContinuation Continuation Object Continuation
                  -- начальное пустое продолжение
                  | EmptyContinuation Environment
                  deriving (Eq, Show)

data Result = OK Object
  | ERROR String
  deriving Eq

instance Show Object where
  show (SYMBOL s) = s
  show (NUM i) = (show i)
  show (LIST []) = "NIL"
  show (LIST l) = "(" ++ (concat $ intersperse " " $ map show l) ++ ")"
  show (LAMBDA args body env) = "LAMBDA " ++ (show args) ++ " " ++ (concat $ intersperse " " $ map show body) ++ " env = " ++ (show env)
  show (PRIM s) = "PRIM " ++ (show s)

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
resume :: Continuation -> Object -> (Object, Environment)

-- обновление переменной в окружении
update :: Environment -> String -> Object -> Environment
update env var val = update' var val env [] where
  update' var val [] env = (var, val):env
  update' var val ((key, obj):tail) env = if key == var then (key, val):tail else
    (key, obj):update tail var val
-- обновление окружения внутри продолжения
updateCont :: Continuation -> String -> Object -> Continuation
updateCont (IfContinuation cc o1 o2 env) var val = IfContinuation cc o1 o2 $ update env var val
updateCont (BeginContinuation cc o env) var val = BeginContinuation cc o $ update env var val
updateCont (SetContinuation cc s env) var val = SetContinuation cc s $ update env var val
updateCont (FunContinuation cc s env) var val = FunContinuation cc s $ update env var val
updateCont (ApplyContinuation cc s env) var val = ApplyContinuation cc s $ update env var val
updateCont (ArgContinuation cc s env) var val = ArgContinuation cc s $ update env var val
updateCont (EmptyContinuation env) var val = EmptyContinuation $ update env var val
updateCont c _ _ = c
-- вычисление аргументов при вызове функции
evalArgs :: [Object] -> Environment -> Continuation -> (Object, Environment)
evalArgs [] _ cc = resume cc $ LIST []
evalArgs (arg:t) env cc = eval arg env $ ArgContinuation cc t env
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
invoke (CONT cc) vals env _ = resume cc $ head vals
-- вызов продолжить  с текущим продолжением
invoke (PRIM "call/cc") vals env cc = invoke (head vals) [CONT cc] env cc
-- вызов примитива
invoke (PRIM f) vals _ cc = resume cc $ prim f vals
invoke _ _ _ _ = error "Invoke"
-- поиск тела функции под динамической меткой
catchLookup :: Continuation -> Object -> Continuation -> (Object, Environment)
catchLookup (IfContinuation cc _ _ _) tag tc = catchLookup cc tag tc
catchLookup (BeginContinuation cc _ _) tag tc = catchLookup cc tag tc
catchLookup (FunContinuation cc _ _) tag tc = catchLookup cc tag tc
catchLookup (ApplyContinuation cc _ _) tag tc = catchLookup cc tag tc
catchLookup (ArgContinuation cc _ _) tag tc = catchLookup cc tag tc
catchLookup (GathContinuation cc _) tag tc = catchLookup cc tag tc
catchLookup (CatchContinuation cc _ _) tag tc = catchLookup cc tag tc
catchLookup (ThrowContinuation cc _ _) tag tc = catchLookup cc tag tc
catchLookup (ThrowingContinuation cc _ _) tag tc = catchLookup cc tag tc
catchLookup lc@(LabelContinuation cc t) tag tc@(ThrowContinuation c2 form env)  =
  if t == tag then eval form env $ ThrowingContinuation tc tag lc
  else catchLookup cc tag tc
catchLookup (EmptyContinuation _) tag _ = error $ "No associated catch: " ++ (show tag)
-- возобновление сохраненного продолжения
resume (IfContinuation cc true _ env) (SYMBOL "T") = eval true env cc
resume (IfContinuation cc _ false env) (LIST []) = eval false env cc
resume (BeginContinuation cc (_:cdr) env) _ = evalBegin cdr env cc
resume (SetContinuation _ var []) _ = error $ "Unknown variable" ++ (show var)
resume (SetContinuation cc var env) val = resume (updateCont cc var val) val
resume (FunContinuation cc args env) f = evalArgs args env $ ApplyContinuation cc f env
resume (ArgContinuation cc args env) val = evalArgs args env $ GathContinuation cc val
resume (GathContinuation cc val) (LIST objs) = resume cc $ LIST (val:objs)
resume (CatchContinuation cc body env) tag = evalBegin body env $ LabelContinuation cc tag
resume c@(ThrowContinuation cc form env) tag = catchLookup c tag c
resume (ThrowingContinuation cc tag c2) v = resume c2 v
resume (LabelContinuation cc tag) obj = resume cc obj
resume (ApplyContinuation cc f env) (LIST vals) = invoke f vals env cc
resume (EmptyContinuation env) obj = (obj, env)
--resume _ _ = error "Resume"

evalQuote :: Object -> Environment -> Continuation -> (Object, Environment)
evalQuote obj env cc = resume cc obj

evalIf :: Object -> Object -> Object -> Environment -> Continuation -> (Object, Environment)
evalIf expr true false env cc = eval expr env $ IfContinuation cc true false env

evalBegin [] env cc = resume cc $ LIST []
evalBegin (car:[]) env cc = eval car env cc
evalBegin (car:cdr) env cc = eval car env $ BeginContinuation cc (car:cdr) env

evalVar :: String -> Environment -> Continuation -> (Object, Environment)
evalVar n [] _ = error $ "Unknown var" ++ (show n)
evalVar n ((var, val):cdr) cc = if n == var then resume cc val
   else evalVar n cdr cc

evalSet :: String -> Object -> Environment -> Continuation -> (Object, Environment)
evalSet var expr env cc = eval expr env $ SetContinuation cc var env
-- -- Ламбда    аргументы
evalLambda :: Object -> [Object] -> Environment -> Continuation -> (Object, Environment)
evalLambda args body env cc = resume cc $ LAMBDA args body env
evalCallcc (LIST (SYMBOL "LAMBDA":args:body)) env cc = invoke (LAMBDA args body env) [CONT cc] env cc
evalCatch tag body env cc = eval tag env $ CatchContinuation cc body env
evalThrow tag form env cc = eval tag env $ ThrowContinuation cc form env

evalApp :: Object -> [Object] -> Environment -> Continuation -> (Object, Environment)
evalApp f args env cc = eval f env $ FunContinuation cc args env

eval (NUM i) env cc = resume cc $ NUM i
eval (LIST []) env cc = resume cc $ LIST []
eval (SYMBOL var) env cc = evalVar var env cc
-- -- особые формы     
eval (LIST (SYMBOL "QUOTE":cdr)) env cc = evalQuote (head cdr) env cc
eval (LIST (SYMBOL "IF":expr:true:false)) env cc = evalIf expr true (head false) env cc
eval (LIST (SYMBOL "BEGIN":cdr)) env cc = evalBegin cdr env cc
eval (LIST (SYMBOL "SETQ":SYMBOL var:expr)) env cc = evalSet var (head expr) env cc
eval (LIST (SYMBOL "LAMBDA":args:body)) env cc = evalLambda args body env cc
eval (LIST (SYMBOL "CALL/CC":lambda:[])) env cc = evalCallcc lambda env cc
eval (LIST (SYMBOL "CATCH":tag:body)) env cc = evalCatch tag body env cc
eval (LIST (SYMBOL "THROW":tag:val:_)) env cc = evalThrow tag val env cc
eval (LIST (car:cdr)) env cc = evalApp car cdr env cc
eval _ _ _ = error "Eval error"

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
      let (res, env) = eval (fst $ last $ ob) e $ EmptyContinuation e
      S.liftIO $ putStrLn (show res)
      S.put env
    process

t = SYMBOL "T"
nil = LIST []
prim "cons" (a:b) = case head b of
  LIST c -> LIST $ [a] ++ c
  _ -> error "invalid list in cons"
prim "cons" _ = error "Invalid cons"
prim "car" (LIST h:_) = head h 
prim "car" _ = error "Invalid car"
prim "cdr" (LIST h:_) = LIST $ tail h
prim "cdr" _ = error "Invalid cdr"
prim "+" (NUM n1:NUM n2:_) = NUM $ n1 + n2
prim _ _ = error "Invalid prim"

globalEnv = [("T", t), ("NIL", nil),
             ("+", PRIM "+"),
             ("CONS", PRIM "cons"),
             ("CAR", PRIM "car"),
             ("CDR", PRIM "cdr")
             ]

repl :: IO ()
repl = do
  let env = globalEnv
  res <- S.runStateT process env
  return ()
main = repl
