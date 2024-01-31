module VM (run) where
import qualified Control.Monad.State as S
import qualified Data.Vector as V
import Types

run :: Code -> S.StateT FrameList IO Object
run (CONST o) = return o
run (VAR_SH j) = do
  fr <- S.get
  return $ head fr V.! j
run (VAR_DEEP i j) = do
  fr <- S.get
  return $ (fr !! i) V.! j
run (GLOBAL 0) = return $ SYMBOL "T"
run (GLOBAL 1) = return $ LIST []
run (IF cond t f) = do
  c <- run cond
  case c of
    SYMBOL "T" -> run t
    LIST [] -> run f
run (SEQ h t) = run h >> run t
run (ALLOC size) = do -- создание кадра активации до применения функции
  fr <- S.get
  S.put $ [V.generate size (\_ -> NUM 0)] ++ fr
  return  $ NUM 0
run (STORE c1 c2 i) = do -- сохраняем значение аргумента в последний кадр
  run c2
  c <- run c1
  fr <- S.get
  S.put $ (head fr V.// [(i, c)]):tail fr
  return c
run (CLOSURE code size) = do
  fr <- S.get
  if (length $ head fr) /= size then error "Incorrect arity" else run code
run (TAIL_CALL f args) = run args >> run f
run (ADD c1 c2) = do
  (NUM n1) <- run c1
  (NUM n2) <- run c2
  return $ NUM $ n1 + n2
