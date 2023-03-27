module Chapter3 where

main :: IO()
main = do
  putStrLn greeting
    where greeting =
            concat [hello, " ", world, "!"]
            where hello = "Hello"
                  world = "world"
