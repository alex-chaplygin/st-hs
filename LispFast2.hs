import qualified Data.Vector as V
import Control.Monad.State
import Parser
import Compiler
import Types
import VM

data Result = OK Object
  | ERROR String
  deriving Eq

process :: GlobalEnv -> IO ()
process env = do
  putStr "> "
  str <- getLine
  if str == "q" then return () else do
    let ob = parse str
    if ob == [] then do
      putStrLn "Ошибка ввода"
    else do
      let (code, env') = runState (meaning (fst $ last $ ob) [] True ) env
      putStrLn $ show code
      let state = execState (run code) $ startState env'
      --putStrLn $ show $ _val state
      process $ _globalEnv state
    process env
-- глобальное окружение
globalEnv = [("T", CONST $ SYMBOL "T"), ("NIL", CONST $ LIST [])]

main :: IO ()
main = process globalEnv
