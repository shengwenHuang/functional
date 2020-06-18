threes::[num] -> num
threes [] = 0
threes (3: xs) = 1 + threes xs
threes (x: xs) = threes xs
