module VM (exec, startState, VMState(..)) where
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad.State
import Control.Lens
import qualified Data.Vector as V
import Types
import Compiler(primitives)

data VMState = VMState
  { _frames :: FrameList -- кадры активации
  , _globalEnv :: GlobalEnv -- глобальное окружение
  , _val :: Object -- регистр значений
  , _stack :: [Object] -- стек значений
  , _callstack :: [Int] -- стек возвратов
  , _pc :: Int -- счетчик команд
  }
makeLenses ''VMState

startState env = VMState
  { _frames = []
  , _globalEnv = env
  , _val = NUM 0
  , _stack = []
  , _callstack = []
  , _pc = 0
  }
-- цикл работы машины
exec code = do
  pc' <- use pc
  let c = code !! pc'
  if c == HALT then return () else do
    run c
    pc += 1
    exec code

run :: Code -> State VMState ()
run (CONST o) = val .= o
run (VAR_SH j) = do
  fr <- use frames
  val .= head fr V.! j
run (VAR_DEEP i j) = do
  fr <- use frames
  val .= (fr !! i) V.! j
--run (SET_VAR_SH j c) = do
--  c' <- run c
--  updateFrame $ (head fr V.// [(j, c')]):tail fr
--  return c'
--run (SET_VAR_DEEP i j c) = do
--  (fr, env) <- get
--  c' <- run c
--  put (deepUpdate fr i j c', env)
----  return c'
run (SET_GLOBAL i) = do
  v <- use val
  env <- use globalEnv
  globalEnv .= globalUpdate env i v
run (GLOBAL i) = do
  env <- use globalEnv
  let (_, o) = env !! i
  val .= o
--run PUSHVAL = use val >>= \v -> use stack -> \s -> stack .= v::s >> return ()
run (GOTO i) = pc .= i
run (JMPFALSE i) = do
  v <- use val
  if v == LIST[] then run (GOTO i) else return ()
run RETURN = use callstack >>= \s -> pc .= head s >> callstack .= tail s
--run (ALLOC size) = do -- создание кадра активации до применения функции
--  (fr, env) <- get
--  put ([V.generate size (\_ -> NUM 0)] ++ fr, env)
--  return  $ NUM 0
--run (STORE c1 c2 i) = do -- сохраняем значение аргумента в последний кадр
--  run c2
--  c <- run c1
--  (fr, env) <- get
--  put ((head fr V.// [(i, c)]):tail fr, env)
--  return c
--run (CLOSURE code size) = do
--  (fr, _) <- get
--  if (length $ head fr) /= size then error "Incorrect arity" else run code
--run (TAIL_CALL f args) = run args >> run f
--run (CALL f args) = do
--  (fr, gl) <- get
--  a <- run args
--  res <- run f
--  (_, gl') <- get
--  put (fr, gl')
--  return res
--run (PRIM0 n) = return $ runPrimitive n []
--run (PRIM1 n c1) = run c1 >>= \c -> return $ runPrimitive n [c]
--run (PRIM2 n c1 c2) = do
--  c1' <- run c1
--  c2' <- run c2
--  return $ runPrimitive n [c1', c2']
--
-- глубокое обновление переменной
deepUpdate :: FrameList -> Int -> Int -> Object -> FrameList
deepUpdate (car:cdr) 0 j v = (car V.// [(j, v)]) : cdr
deepUpdate (car:cdr) i j v = car : deepUpdate cdr (i-1) j v
-- обновление глобального окружения
globalUpdate ((n,_):cdr) 0 c = (n,c) : cdr
globalUpdate (car:cdr) i c = car : globalUpdate cdr (i-1) c
-- запуск примитива
runPrimitive n args = let (_,_,p) = primitives !! n in p args
