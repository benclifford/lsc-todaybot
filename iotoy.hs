import Control.Monad.Freer
import IOEffect

main = runIO program

program = do
  send (IOEffect $ putStrLn "hello")
  send (IOEffect $ putStrLn "middle")
  send (IOEffect $ putStrLn "goodbye")

