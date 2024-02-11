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
      let (env', code) = execState (meaning (fst $ last $ ob) [] True ) (env, [])
      putStrLn $ show code
      let state = execState (mapM_ run code) $ startState env'
      putStrLn $ show $ _val state
      process $ _globalEnv state
-- глобальное окружение
globalEnv = [("T", SYMBOL "T"), ("NIL", LIST [])]

main :: IO ()
main = process globalEnv
