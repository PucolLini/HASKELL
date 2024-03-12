import Text.Read (readMaybe)

readSet :: IO [Int]
readSet = do
    putStrLn "Enter a set of numbers separated by spaces:"
    input <- getLine
    let maybeSet = mapM readMaybe (words input) :: Maybe [Int]
    case maybeSet of
        Just s -> return s
        Nothing -> do
            putStrLn "Invalid input. Please enter a valid set of numbers."
            readSet

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)

main :: IO ()
main = do
    listL <- readSet
    putStrLn $ "Original set: " ++ show listL
    putStrLn $ "Power set: " ++ show (powerSet listL)
