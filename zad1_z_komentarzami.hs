type Triple = (Int, Int, Int)

isPythagorean :: Triple -> Bool
isPythagorean (a, b, c) = a*a + b*b == c*c || a*a + c*c == b*b || b*b + c*c == a*a

triples :: Int -> [Triple]
triples n = [(i, j, k) | i <- [1..n], j <- [i..n], k <- [j..n], isPythagorean (i, j, k), i + j + k < n]

-- | - określa warunek, rozdziela różne przypadki
-- || - operator logiczny OR

countSums :: Int -> [Triple] -> [(Int, Int)]
countSums n ts = [(s, count s ts) | s <- [1..n]]
  where
    count x = length . filter ((==x) . sumTriple)
    sumTriple (a, b, c) = a + b + c

-- countSums n ts = [(s, count s ts) | s <- [1..n]]
-- s musi być wartością między 1 do n
-- count s ts - liczy ile jest s w ts (liście trójek liczb całkowitych - Triple)
-- (s, count s ts) tworzy parę liczb (Int, Int) jako wynik dla ts
-- dla każdego s twrzono jest para s, count s ts

-- where - definiuje dodatkowe warunki
-- count - definiuje funkcji count, przyjmujaca x jako argument i zwraca liczbę wystąpień sumy x w liście trójek ts

-- length . filter ((==x) . sumTriple)
-- filter ((==x) . sumTriple)
-- filter przepuszcza tylko trójki dla których sumTriple (suma trzech liczb) jest równa x
-- ==x  czy jest równe x
-- length zlicza ile trójek zostało przepuszczonych przez filter - daje to liczbę wystąpien sumy x

-- sumTriple (a, b, c) = a + b + c - definiuje funkcję sumTriple
-- przyjmuje trójkę liczb a b c i zwraca ich sumę

-- countSums - tworzy listę par liczb, gdzie pierwszy element pary to suma, a drugi to ilość wystąpień tej sumy w liście trójek.
-- count - to funkcja pomocnicza, która zlicza wystąpienia danej sumy.
-- sumTriple - to funkcja pomocnicza, która zwraca sumę trzech liczb.

maxCount :: [(Int, Int)] -> Int
maxCount = maximum . map snd

-- maxCount = maximum . map snd
-- maxCount to definicja funkcji
-- map snd to funkcja, bierze ona liste par (int, int) i zwraca listę drugich elementów z każdej pary
-- maximum funkcja, bierze listę liczb całkowityhch i zwraca największą z nich

-- . - łączy dwie funkcje

-- czyli ostatecznie maxCount:
-- bierze listę par liczb
-- przekształca tę liste aby uzsykać listę drugich elementów z par
-- znajduje największą wartość w tej nowej liście liczb

main :: IO ()
main = do
  let n = 100
  let ts = triples n
  let counts = countSums n ts
  let max = maxCount counts
  let maxSums = [s | (s, c) <- counts, c == max]
  print maxSums 


-- counts to lista par liczb (Int, Int). Każda para składa się z dwóch elementów: s i c.
-- | oznacza "gdzie" lub "taki, że". W tym przypadku, można przetłumaczyć jako "dla których".

-- (s, c) <- counts to destrukturyzacja, dzięki której dla każdej pary (s, c) w liście counts, s przyjmuje wartość pierwszego elementu pary, a c przyjmuje wartość drugiego elementu pary.

-- c == max to warunek filtrujący. Oznacza to, że tylko te pary, dla których wartość drugiego elementu (c) jest równa wartości max, są brane pod uwagę.

-- [s | (s, c) <- counts, c == max] to właśnie konstrukcja nowej listy. Dla każdej pary (s, c) w counts, gdzie c jest równe max, do nowej listy dodawane jest tylko pierwsze elementy tych par (s).

--Podsumowując, maxSums to lista zawierająca tylko te wartości s, które odpowiadają maksymalnej wartości c w oryginalnej liście counts. W praktyce, są to sumy, dla których liczba wystąpień była największa.
