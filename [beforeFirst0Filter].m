beforeFirst0 :: [num] -> (num, num, num)
beforeFirst0 input = count (takewhile notZero input)

count :: [num] -> (num, num, num)
count input 
           = (#(getOnes input), #(getTwos input), (seqOnes input 0 0))

notZero :: num -> bool
notZero any = True, if any ~= 0
            = False, otherwise

getOnes = filter (= 1)

getTwos = filter (= 2)

seqOnes :: [num] -> num -> num -> num
seqOnes []      longest current = max[longest, current]
seqOnes (1: xs) longest current = seqOnes xs longest (current + 1)
seqOnes (2: xs) longest current = seqOnes xs (max[longest, current]) 0
seqOnes any     longest current = error"bad input format"
