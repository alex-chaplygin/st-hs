import qualified Data.Vector as V
import Control.Monad.State
import Parser
import Compiler
import Types
import VM

data Result = OK Object
  | ERROR String
  deriving Eq

process :: StateT (FrameList, GlobalEnv) IO ()
process = do
  lift $ putStr "> "
  str <- lift $ getLine
  if str == "q" then return () else do
    let ob = parse str
    if ob == [] then do
      lift $ putStrLn "Ошибка ввода"
    else do
      code <- meaning (fst $ last $ ob) [] True
      lift $ putStrLn (show code)
      res <- run code
      lift $ putStrLn (show res)
    process
-- глобальное окружение
globalEnv = [("T", CONST $ SYMBOL "T"), ("NIL", CONST $ LIST [])]

main :: IO ()
main = do
  res <- runStateT process ([], globalEnv)
  return ()
