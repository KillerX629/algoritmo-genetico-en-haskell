import Control.Monad(forM,forM_)

main::IO()
main = do
  let askThenGreet i = do
      putStrLn $ "What's your name (" ++ (show i) ++ ")"
      name <- getLine
      putStrLn $ "Hello! " ++ name
  forM [1,2,3] askThenGreet