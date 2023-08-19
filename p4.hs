cartProd :: [a] -> [b] -> [(a,b)]
cartProd [] _ = []
cartProd _ [] = []
cartProd (x:xs) ys = zip (repeat x) ys ++ (cartProd xs ys)

largestPalindrome :: (Integral a, Show a) => a
largestPalindrome = maximum $ filter isPalindrome products
    where
        isPalindrome n = reverse (show n) == show n
        threeDigNums = [999,998..100]
        products = map (\(a,b) -> a*b) (cartProd threeDigNums threeDigNums)
