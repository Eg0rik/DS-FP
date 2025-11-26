### Решение задач из презентации 6
```haskell
-- 1. Развернуть натуральное число в список всех чисел, меньших его
numbersLessThan :: Int -> [Int]
numbersLessThan n = [0..n-1]

-- 2. Развернуть число в список разрядов его двоичного представления
toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = reverse (toBinary' n)
  where
    toBinary' 0 = []
    toBinary' x = (x `mod` 2) : toBinary' (x `div` 2)

-- 3. Список разрядов преобразовать свёрткой в значение числа
fromBinary :: [Int] -> Int
fromBinary = foldl (\acc digit -> acc * 2 + digit) 0

-- 4. Развернуть число в список его простых делителей
isPrime :: Int -> Bool
isPrime 1 = False
isPrime x = all (\d -> x `mod` d /= 0) [2..floor (sqrt (fromIntegral x))]

primeDivisors :: Int -> [Int]
primeDivisors n = filter (\x -> n `mod` x == 0 && isPrime x) [2..n]

-- 5. Список первых n чисел Фибоначчи через развёртку
fibonacci :: Int -> [Int]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- 6. Бесконечный список чисел Фибоначчи
infiniteFibs :: [Integer]
infiniteFibs = 0 : 1 : zipWith (+) infiniteFibs (tail infiniteFibs)

-- 7. Развернуть число в сиракузскую последовательность
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
  | even n    = n : collatz (n `div` 2)
  | otherwise = n : collatz (3 * n + 1)

-- Тестирование всех функций
main :: IO ()
main = do
    putStrLn "1. numbersLessThan 5:"
    print $ numbersLessThan 5
    
    putStrLn "\n2. toBinary 10:"
    print $ toBinary 10
    
    putStrLn "\n3. fromBinary [1,0,1,0]:"
    print $ fromBinary [1,0,1,0]
    
    putStrLn "\n4. primeDivisors 28:"
    print $ primeDivisors 28
    
    putStrLn "\n5. fibonacci 10:"
    print $ fibonacci 10
    
    putStrLn "\n6. Первые 10 чисел Фибоначчи из бесконечного списка:"
    print $ take 10 infiniteFibs
    
    putStrLn "\n7. collatz 6:"
    print $ collatz 6
```

### Результат в консоли:
Запускал код в онлайн компиляторе - https://play.haskell.org/
<img width="640" height="409" alt="image" src="https://github.com/user-attachments/assets/388634de-7d90-4aaa-8e91-0cc48f4899c4" />
