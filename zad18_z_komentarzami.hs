slownikSetek :: [Int]
slownikSetek = [0,3,8,7,9,7,8,9,8,11]

slownikJednosci :: [Int]
slownikJednosci = [0,5,3,4,6,4,4,6,5,7,8,9,8,9,10,9,9,11,10,12]

slownikDziesiatek :: [Int]
slownikDziesiatek = [0,8,9,11,12,12,12,14,13,16]

calculateLength :: Int -> Int
calculateLength n
    | n < 20 = slownikJednosci !! n
    | n < 100 = slownikDziesiatek !! (n `div` 10) + slownikJednosci !! (n `mod` 10)
    | otherwise = slownikSetek !! (n `div` 100) + calculateLength (n `mod` 100)

-- !! uzyskanie elementu z listy na podstawie jego indeksu
-- czyli: list !! index to to samo co lista[index] (indeksowanie od 0)

-- + dodawanie dwóch liczb

-- Int -> Int Przyjmuje Inta jako argument a drugiego inta (po prawej) daje jako wynik funkcji
-- w przypadku Int1 -> Int2 -> Int3 funkcja przyjmuje Int1 i Int2 jako argumenty i Int3 jest typem zwracanego wyniku

-- :: znacznik deklaracji, zdefiniowanie typu wyrażenia, funkcji lub wartości - czyli przypisanie typu

main :: IO ()
main = do
    putStrLn "Enter a number between 1 and 999:"
    n <- readLn
    let length = calculateLength n
    putStrLn $ "The length of the word representation of the number is: " ++ show length


-- IO() IO - input/output, () - nie zwraca żadnej wartości


