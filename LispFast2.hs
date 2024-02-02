import qualified Data.Vector as V
import qualified Control.Monad.State as S
import Parser
import Compiler
import Types
import VM

data Result = OK Object
  | ERROR String
  deriving Eq

process :: S.StateT (FrameList, GlobalEnv) IO ()
process = do
  S.liftIO $ putStr "> "
  str <- S.liftIO $ getLine
  if str == "q" then return ()
    else do
    let ob = parse str
    if ob == [] then do
      S.liftIO $ putStrLn "Ошибка ввода"
    else do
      code <- meaning (fst $ last $ ob) [] True
      S.liftIO $ putStrLn (show code)
      res <- run code
      S.liftIO $ putStrLn (show res)
    process
-- глобальное окружение
globalEnv = [("T", SYMBOL "T"), ("NIL", LIST [])]

main :: IO ()
main = do
  res <- S.runStateT process ([], globalEnv)
  return ()
