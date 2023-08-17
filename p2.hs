fibsum :: (Integral a) => a -> a
fibsum x = fibsum' (1, 1, x, 0)
    where fibsum' (prev, cur, max, sum) 
              | cur > max = sum
              | otherwise = fibsum' (cur, prev + cur, max, sum + evenOrZero cur)
              where evenOrZero x 
                        | x `mod` 2 == 0 = x
                        | otherwise      = 0

-- Run: fibsum 4000000
-- Answer: 4613732
