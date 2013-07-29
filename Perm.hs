perm 0 = [[]]
perm n = concat (map (allInsert n) (perm (n-1))

allInsert n xs = allInsert' n [] xs

allInsert' n xs [] = [xs++[n]]
allInsert' n xs (y:ys) = (xs++[n]++(y:ys)):(allInsert' n (xs++[y]) ys)