import System.IO

stringToInt str = read str::Int

maxDepth [] = 0
maxDepth [_] = 0
maxDepth (x1:x2:xs) = (if x2 > x1 then 1 else 0) + maxDepth (x2:xs)

evalPart1 report = maxDepth (lines report)

window a b c = a + b + c

slidingDepthSums xs = zipWith3 window xs (tail xs) (tail $ tail xs)


main = do
        report <- readFile "input.txt"
        let depths = map stringToInt (lines report)
        let totalDepthPt1 = maxDepth depths
        print ("PART 1: " ++ show totalDepthPt1)

        let totalDepthPt2 = maxDepth $ slidingDepthSums depths
        print ("PART 2: " ++ show totalDepthPt2)

        