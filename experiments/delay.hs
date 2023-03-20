import Control.Concurrent (threadDelay)

countdown :: Int -> IO ()
countdown 0 = putStrLn "Blast off!"
countdown n = do
  putStrLn $ "Countdown: " ++ show n
  threadDelay 1000000  -- delay for 1 second (1000000 microseconds)
  countdown (n - 1)
