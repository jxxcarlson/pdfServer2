
printNumbers :: Int -> IO ()
printNumbers n
  | n > 10 = return ()  -- stop recursion when n > 10
  | otherwise = do
      print n
      printNumbers (n + 1)  -- recursively call printNumbers with the next number