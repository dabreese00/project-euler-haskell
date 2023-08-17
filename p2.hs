evenOrZero :: (Integral a) => a -> a
evenOrZero x = if x `mod` 2 == 0 then x else 0

fibsum' :: (Integral a) => (a, a, a, a) -> a
fibsum' (prev, cur, max, sum) = if cur > max then sum
                                   else fibsum' (cur, prev + cur, max, sum + evenOrZero cur)

fibsum :: (Integral a) => a -> a
fibsum x = fibsum' (1, 1, x, 0)

-- Run: fibsum 4000000
-- Answer: 4613732
