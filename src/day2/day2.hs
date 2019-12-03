input = [1, 12, 2, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 13, 1, 19, 1, 9, 19, 23, 2, 23, 13, 27, 1, 27, 9, 31, 2, 31, 6, 35, 1, 5, 35, 39, 1, 10, 39, 43, 2, 43, 6, 47, 1, 10, 47, 51, 2, 6, 51, 55, 1, 5, 55, 59, 1, 59, 9, 63, 1, 13, 63, 67, 2, 6, 67, 71, 1, 5, 71, 75, 2, 6, 75, 79, 2, 79, 6, 83, 1, 13, 83, 87, 1, 9, 87, 91, 1, 9, 91, 95, 1, 5, 95, 99, 1, 5, 99, 103, 2, 13, 103, 107, 1, 6, 107, 111, 1, 9, 111, 115, 2, 6, 115, 119, 1, 13, 119, 123, 1, 123, 6, 127, 1, 127, 5, 131, 2, 10, 131, 135, 2, 135, 10, 139, 1, 13, 139, 143, 1, 10, 143, 147, 1, 2, 147, 151, 1, 6, 151, 0, 99, 2, 14, 0, 0] :: [Int]


computeResultForOpCode :: Int -> Int -> Int -> Int
computeResultForOpCode opcode val1 val2 = if opcode == 1
                                          then val1 + val2
                                          else val1 * val2

replaceValueInArray :: Int -> Int -> [Int] -> [Int]
replaceValueInArray value position currentArray = take position currentArray ++ [value] ++ drop (position + 1) currentArray

slice :: Int -> Int -> [Int] -> [Int]
slice from to xs = take (to - from) (drop from xs)

getArrayForPosition :: [Int] -> Int -> [Int]
getArrayForPosition array position = slice (position * 4) ((position + 1) * 4) array

computeNextArray :: [Int] -> Int -> [Int]
computeNextArray array position = do
  let currentSubArray = getArrayForPosition array position
  let opCode = head currentSubArray
  if opCode == 99
    then array
    else do
     let firstValue = array !! (currentSubArray !! 1)
     let secondValue = array !! (currentSubArray !! 2)
     let destinationCell = currentSubArray !! 3
     let resultForOpCode = computeResultForOpCode opCode firstValue secondValue
     computeNextArray (replaceValueInArray resultForOpCode destinationCell array) (position + 1)


computeFirstStep :: Int
computeFirstStep = head (computeNextArray input 0)

computeSecondStep :: [Int]
computeSecondStep = [100 * x + y
                    | x <- [0..99], y <- [0..99],
                    head (computeNextArray (replaceValueInArray x 1 (replaceValueInArray y 2 input)) 0) == 19690720]
