module Test.Forever (main) where


import Control.Concurrent (threadDelay)

main :: IO ()
main = loop 6
  where
    loop 0 = putStrLn "terminated"
    loop n = do
        putStrLn $ "run " ++ show n
        threadDelay (5 * 1000 * 1000)
        loop (n - 1)



