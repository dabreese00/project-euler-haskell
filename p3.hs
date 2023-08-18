factorize :: (Integral a) => a -> [a]
factorize n
    | factors == [] = [n]
    | otherwise     = (factorize firstFactor) ++ (factorize secondFactor)
    where
        factors = filter (\x -> n `mod` x == 0) [2..(n-1)]
        firstFactor = head factors
        secondFactor = n `div` firstFactor

largestPrimeFactor :: (Integral a) => a
largestPrimeFactor =
    let n = 600851475143
    in last $ factorize n
