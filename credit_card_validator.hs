double_num x = x * 2

-- filter out wrong answers in functional programming
to_arr x = if x > 10 then (to_arr (div x 10)) ++ ([mod x 10]) else [x]




double_every_other :: [Int] -> [Int]
double_every_other xs = reverse [if (mod (snd x) 2) == 0 then fst x * 2 else fst x | x <- reverse (zip xs [0..])]

sum_digits :: Int -> Int
sum_digits x = if x > 10 then ((mod x 10) + (sum_digits (div x 10))) else x

sum_digits_ls :: [Int] -> Int
sum_digits_ls xs = sum [sum_digits x | x <- xs]

is_valid :: Int -> Bool
is_valid x = if (mod (sum_digits_ls (double_every_other (to_arr x))) 10) == 0 then True else False

