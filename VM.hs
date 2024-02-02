module VM (run) where
import qualified Control.Monad.State as S
import qualified Data.Vector as V
import Types

run :: Code -> S.StateT (FrameList, GlobalEnv) IO Object
run (CONST o) = return o
run (VAR_SH j) = do
  (fr, _) <- S.get
  return $ head fr V.! j
run (VAR_DEEP i j) = do
  (fr, _) <- S.get
  return $ (fr !! i) V.! j
run (SET_VAR_SH j c) = do
  (fr, env) <- S.get
  c' <- run c
  S.put ((head fr V.// [(j, c')]):tail fr, env)
  return c'
run (SET_VAR_DEEP i j c) = do
  (fr, env) <- S.get
  c' <- run c
  S.put (deepUpdate fr i j c', env)
  return c'
run (SET_GLOBAL i c) = do
  (fr, env) <- S.get
  c' <- run c
  S.put (fr, globalUpdate env i c')
  return c'
run (GLOBAL i) = do
  (_, env) <- S.get
  let (_, o) = env !! i in return o
run (IF cond t f) = do
  c <- run cond
  case c of
    SYMBOL "T" -> run t
    LIST [] -> run f
run (SEQ h t) = run h >> run t
run (ALLOC size) = do -- создание кадра активации до применения функции
  (fr, env) <- S.get
  S.put ([V.generate size (\_ -> NUM 0)] ++ fr, env)
  return  $ NUM 0
run (STORE c1 c2 i) = do -- сохраняем значение аргумента в последний кадр
  run c2
  c <- run c1
  (fr, env) <- S.get
  S.put ((head fr V.// [(i, c)]):tail fr, env)
  return c
run (CLOSURE code size) = do
  (fr, _) <- S.get
  if (length $ head fr) /= size then error "Incorrect arity" else run code
run (TAIL_CALL f args) = run args >> run f
run (ADD c1 c2) = do
  (NUM n1) <- run c1
  (NUM n2) <- run c2
  return $ NUM $ n1 + n2
run (MUL c1 c2) = do
  (NUM n1) <- run c1
  (NUM n2) <- run c2
  return $ NUM $ n1 * n2
-- глубокое обновление переменной
deepUpdate :: FrameList -> Int -> Int -> Object -> FrameList
deepUpdate (car:cdr) 0 j v = (car V.// [(j, v)]) : cdr
deepUpdate (car:cdr) i j v = car : deepUpdate cdr (i-1) j v

globalUpdate ((n,_):cdr) 0 c = (n,c) : cdr
globalUpdate (car:cdr) i c = car : globalUpdate cdr (i-1) c
