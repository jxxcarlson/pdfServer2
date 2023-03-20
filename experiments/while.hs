Haskell doesn't have a built-in while loop like some other programming languages do, but you can use recursion to achieve a similar effect.

Here's an example of a recursive function that uses a "while loop" to repeatedly print the numbers from 1 to 10:

lua
Copy code
printNumbers :: Int -> IO ()
printNumbers n
  | n > 10 = return ()  -- stop recursion when n > 10
  | otherwise = do
      print n
      printNumbers (n + 1)  -- recursively call printNumbers with the next number
