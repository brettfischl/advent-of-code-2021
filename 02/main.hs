import System.IO
import Data.List.Split

calculateVector inst = splitOn " "

calculatePos [] height depth = (height, depth)
calculatePos (i:is) height depth = do
                          let intruction:vector:_ = splitOn " " i

                          if intruction == "forward" then calculatePos is height (depth + read vector::Int)
                          else if intruction == "down" then calculatePos is (height + read vector::Int) depth
                          else calculatePos is (height - read vector::Int) depth


calculateAimedPos [] distance depth aim = (distance, depth)
calculateAimedPos (i:is) distance depth aim = do
                                              let intruction:vector:_ = splitOn " " i
                                              
                                              if intruction == "up" then calculateAimedPos is distance depth (aim - read vector::Int)
                                              else if intruction == "down" then calculateAimedPos is distance depth (aim + read vector::Int)
                                              else calculateAimedPos is (distance + read vector::Int) (depth + (aim * read vector::Int)) aim


main = do
        report <- readFile "./02/input.txt"
        let instructions = lines report

        let (distance, depth) = calculatePos instructions 0 0

        print ("PART 1 " ++ show (distance * depth))


        let (distance, depth) = calculateAimedPos instructions 0 0 0
        print ("PART 2: " ++ show (distance * depth))