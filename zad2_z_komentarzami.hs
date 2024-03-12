import Text.Read (readMaybe)

-- importowanie Text.Read czyli modułu w bibliotece standardowej haskella
-- readMaybe - bezpieczna konwersja napisów na wartości (w przypadku błędu zamiast errora da Nothing (NULLa))

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

-- mapM readMaybe (words input) - parsowanie, zamienianie inputu który jest stringiem na Inta (bezpieczna zamiana bez errorów)

-- case maybeSet of obsługiwanie wyników parsowania
-- w przypadku Just s (s to lista liczb) to zwraca tę listę
-- w przypadku gdy chociaż dla jednego elementu parsowaniesię nie powiedzie wystąpi drugi przypadek

-- Just konstruktor danych używanych w Maybe
-- Maybe do reprezentowanie wartości które mogą być obecne lub nie (czyli Just lub Nothing)
-- Just s - wartość obecna
-- Nothing - wartość nieobecna


powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)

-- generowanie zbioru potęgowego dla danej listy
-- [] - lista
-- [a] - lista elementow typu a 
-- [[a]] - lista listy elementów typu a

-- a - w sygnaturze tej funkcji jest zmienną ogólną, oznaczająca, że funkcja działa dla list elementów dowolnego typu

-- w przypadku otrzymania pustej listy zwraca pusta liste list ([[]]) czyli tak naprawde jednoelementową listę który jest zbiór pusty
-- w przypadku otrzymania poprawnej listy (którą można przedstawić jako x:xs) to :
-- rekurencyjnie generuje zbiór potęgowy dla xs
-- dołącza do niego zbiór potęgowy xs
-- i zbiór który powstaje przez dodanie x do każdego podzbioru zbioru potęgowego xs

-- (x:xs) - wzorzec stosowany do list
-- x - pierwszy element listy
-- xs - reszta listy (cała lista bez pierwszego elementu)
-- np. numbers (x:xs), gdzie numbers = [1,2,3,4] to x=1, a xs=[2,3,4]

-- map (x:) (powerSet xs) - dodanie x do każdego elementu podzbioru z powerSet xs

-- ++ łączy dwa podzbiory w jeden

main :: IO ()
main = do
    listL <- readSet
    putStrLn $ "Original set: " ++ show listL
    putStrLn $ "Power set: " ++ show (powerSet listL)

-- show listL konwertuje listę na napis STRING

-- $ eliminuje konieczność używania nawiasów (czysto estetyczny zabieg, można równie dobrze zastąpić to nawiasami)
-- czyli jest to równoznaczne z tym kodem:  putStrLn ("Original set: " ++ show listL)

