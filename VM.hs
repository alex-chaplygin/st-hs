module VM (run) where
import qualified Control.Monad.State as S
import Types

run (CONST o) = return o
--run (VAR_SH i) = do
--  fr <- S.get
--  return $ head fr !! i
