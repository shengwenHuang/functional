suit ::= Heart | Diamond | Club | Spade
myCard == (suit, num)

toEven :: [myCard] -> [myCard]
toEven [] = error "no card to make the number even"
toEven input = take (#input - 1) input, if (#input mod 2 = 1)
toEven input = input, otherwise

half :: [myCard] -> ([myCard], [myCard])
half [] = error "no card to half"
half input = (take (entier (#input / 2)) input, drop (entier (#input / 2)) input)

interleave :: ([myCard], [myCard]) -> [myCard] -> [myCard]
interleave ([], []) [] = error "no card to interleave"
interleave (t: top, b: bottom) [] = interleave (top, bottom) [b, t]
interleave (t: top, b: bottom) stack = interleave (top, bottom) (stack ++ [b, t])
interleave ([], []) stack = stack
||interleave ([], any) stack = error "first half is empty"
||interleave (any, []) stack = error "second half is empty"

shuffle :: num -> [myCard] -> [myCard]
shuffle n [] = error "no card"
shuffle n stack = shuffle (n - 1) (interleave (half (toEven stack)) []), if n > 0
                = stack, otherwise

myStack = [(Heart, 1), (Club, 3), (Spade, 13), (Heart, 7), (Diamond, 10), (Club, 8)]
