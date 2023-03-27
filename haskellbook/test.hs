module Test where
sayHello :: String -> IO ()
sayHello x =
  putStrLn (greeting ++ x ++ "!")
  where greeting = "Hello, "
z = 7
y = z + 8
x = y^2
waxOn = x * 5
