import Data.Char

-- problem 1
problem_1 :: Integer
problem_1 = sum . filter (\x -> (mod x 3) == 0 || (mod x 5) == 0) $ [1..999]


-- problem 2
build_list :: [Integer] -> [Integer]
build_list l@(x:y:xs) = if x + y > 4_000_000 then l else build_list $ (x+y):l

problem_2 :: Integer
problem_2 = sum . filter even . build_list $ [2, 1]


-- problem 3
const_3 :: Integer
const_3 = 600851475143

get_prime_factors :: Integer -> Integer -> [Integer] -> [Integer]
get_prime_factors n fac res = if fac * fac > n then
        (n : res)
    else
        if mod n fac == 0 then
            get_prime_factors (div n fac) fac (fac : res)
        else
            get_prime_factors n (fac + 1) res

problem_3 :: Integer
problem_3 = if prime_factors_3 == [] then const_3 else head prime_factors_3
    where prime_factors_3 = get_prime_factors const_3 2 []


-- problem 4
is_palindromic :: String -> Bool
is_palindromic [] = True
is_palindromic [x] = True
is_palindromic s = if head s /= last s then False else
    is_palindromic . tail . init $ s

problem_4 :: Integer
problem_4 = maximum [i * j | i <- [100..999], j <- [100..999], is_palindromic . show $ i * j]


-- problem 5
f5 :: Integer -> Integer -> [Integer] -> Integer
f5 n res [] = res
f5 n res factors@(x:xs) = if mod n x == 0 then f5 (div n x) res xs else f5 n (res * x) xs

calc_5 :: Integer -> Integer
calc_5 2 = 2
calc_5 n = if mod n prev == 0 then prev
    else
        f5 prev prev (get_prime_factors n 2 [])
    where prev = calc_5 (n-1)

problem_5 :: Integer
problem_5 = calc_5 20


-- problem 6
problem_6 :: Integer
problem_6 = (sum [1..100])^2 - sum [i^2 | i <- [1..100]]


-- problem 7
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve l@(x:xs) = x : (sieve . filter (\y -> (mod y x) /= 0) $ xs)

problem_7 :: Integer
problem_7 = sieve [2..] !! (10001 -1)


-- problem 8
const_8 :: String
const_8 = show 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

process_str :: [Char] -> Integer
process_str [] = -1
process_str s@(x:xs) = max (product prefix) (process_str xs)
    where
        prefix = map (\x -> toInteger (ord x) - toInteger (ord '0')) $ take 13 s

problem_8 :: Integer
problem_8 = process_str const_8


-- problem 9
check_int :: RealFrac a => a -> Bool
check_int a = ceiling a == floor a

problem_9 :: Integer
problem_9 = head [a * b * round c | a <- [1..334], b <- [(a+1)..1000], let c = sqrt $ fromIntegral (a^2 + b^2), check_int c, a + b + round c == 1000, round c > b]


-- problem 10
minus :: [Integer] -> [Integer] -> [Integer]
minus (x:xs) (y:ys) = case (compare x y) of 
    LT -> x : (minus xs (y:ys))
    EQ -> minus xs ys
    GT -> minus (x:xs) ys

primes :: [Integer]
primes = 2 : sieve [3..] 4 primes
               where
               sieve (x:xs) q (p:t)
                 | x < q     = x : sieve xs q (p:t)
                 | otherwise =     sieve (minus xs [q, q+p..]) (head t^2) t

problem_10 :: Integer
problem_10 = sum . takeWhile (\x -> x < 2_000_000) $ primes
