type Triple = (Int, Int, Int)

isPythagorean :: Triple -> Bool
isPythagorean (a, b, c) = a*a + b*b == c*c || a*a + c*c == b*b || b*b + c*c == a*a

triples :: Int -> [Triple]
triples n = [(i, j, k) | i <- [1..n], j <- [i..n], k <- [j..n], isPythagorean (i, j, k), i + j + k < n]

countSums :: Int -> [Triple] -> [(Int, Int)]
countSums n ts = [(s, count s ts) | s <- [1..n]]
  where
    count x = length . filter ((==x) . sumTriple)
    sumTriple (a, b, c) = a + b + c


maxCount :: [(Int, Int)] -> Int
maxCount = maximum . map snd

main :: IO ()
main = do
  let n = 100
  let ts = triples n
  let counts = countSums n ts
  let max = maxCount counts
  let maxSums = [s | (s, c) <- counts, c == max]
  print maxSums 
