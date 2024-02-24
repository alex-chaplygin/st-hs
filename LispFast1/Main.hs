import qualified Data.Vector as V
import Control.Monad.State
import Parser
import Interpreter
import Types
import Env

process :: Env -> IO ()
process env = do
  putStr "> "
  str <- getLine
  if str == "q" then return () else do
    let ob = parse str
    if ob == [] then do
      putStrLn "Ошибка ввода"
    else do
      let (res, env') = meaning (fst $ last $ ob) env [] $ \o e -> (o, e)
      putStrLn $ show res
      process env'
-- глобальное окружение
globalEnv = [("T", SYMBOL "T"), ("NIL", LIST [])]

main :: IO ()
main = process []
