import qualified Data.Vector as V
import qualified Control.Monad.State as S
import Parser
import Compiler
import Types (Object(..))

data Result = OK Object
  | ERROR String
  deriving Eq

process :: S.StateT Int IO ()
process = do
  S.liftIO $ putStr "> "
  str <- S.liftIO $ getLine
  if str == "q" then return ()
    else do
    let ob = parse str
    if ob == [] then do
      S.liftIO $ putStrLn "Ошибка ввода"
      else do
      let res = meaning (fst $ last $ ob) [] True in
        S.liftIO $ putStrLn (show res)
    process

repl :: IO ()
repl = do
  res <- S.runStateT process 0
  return ()
main = repl
