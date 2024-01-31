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
--run (TAIL_CALL f args) = let f' = run f
--                             args' = run args in invoke f' args'

--invoke (CLOSURE code size) = run code
