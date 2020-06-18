beforeFirst0 :: [num] -> (num, num, num)
beforeFirst0 input = count(input, (0, 0, 0), 0)

count :: ([num], (num, num, num), num) -> (num, num, num)
count ([], (ones, twos, seqOnes), trackOne) = (ones, twos, seqOnes)
count ((x: xs), (ones, twos, seqOnes), trackOne)
                 = count(xs, (1 + ones, twos, seqOnes), trackOne + 1), if x = 1
                 = count(xs, sequence1((ones, 1 + twos, seqOnes), trackOne), 0)
                                                       , if x = 2
                 = (ones, twos, seqOnes), otherwise

sequence1 :: ((num, num, num), num) -> (num, num, num)
sequence1 ((ones, twos, seqOnes), trackOne) = (ones, twos, seqOnes)
                                                , if trackOne < seqOnes
                                          = (ones, twos, trackOne)
                                                , otherwise

main =  beforeFirst0 [1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 2, 1, 0, 1, 2]
