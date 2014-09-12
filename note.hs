-- YAHT 3.5
-- find the biggest element of a list
biggest [] = 0
biggest (x:xs) =  foldr max 0 (x:xs)


-- YAHT 3.7
-- fibonacci 
fb::Int -> Int
fb x =
    case x of
        1 -> 1
        2 -> 1
        _->(fb (x-1) )+ (fb (x-2))

-- YAHT 3.8
-- recursive multiplication
mult::Int->Int->Int
mult a b = 
    if  b == 1
        then a
        else a + (mult a (b-1))
-- YAHT 3.9
-- my_map
my_map::(a->b)->[a]->[b]
my_map op [] = []
my_map op (x:xs) = (op x): (my_map op xs)

--additionally
-- A
my_insert::(Ord a)=>a->[a]->[a]
my_insert a [] = [a]
my_insert a (x:xs) = 
    if a < x
        then a:x:xs
        else  x:(my_insert a xs)

--B
-- sort using my_insert
my_sort xs =  foldr my_insert [] xs

--C
-- my_take 
my_take n [] = []
my_take n (x:xs)=
    if n == 1
        then [x]
        else x:(my_take (n-1) xs)

--D
--drop n
my_drop n [] = []
my_drop n (x:xs)=
    if n == 0
        then (x:xs)
        else (my_drop (n-1) xs)

paraN :: (Num a, Eq a) => (t -> a -> t) -> t -> a -> t
paraN s z 0 = z
paraN s z n = s (paraN s z (n-1)) (n-1)

fn n = paraN (\r a -> r * (a + 1) ) 1 n

data Dbt x = Lf x|Nda (Dbt x) (Dbt x)|Ndb (Dbt x) (Dbt x)

cataDbt::(x->t)->(t->t->t)->(t->t->t)->Dbt x->t
cataDbt ls as bs (Lf x) = ls x
cataDbt ls as bs (Nda b1 b2) = as (cataDbt ls as bs b1) (cataDbt ls as bs b2)
cataDbt ls as bs (Ndb b1 b2) = bs (cataDbt ls as bs b1) (cataDbt ls as bs b2)


unfold next ended seed =
    if ended seed then []
    else
        let (nx, nseed) = next seed
        in nx : unfold next ended nseed

from a b = unfold (\s -> (s,s+1)) (>b) a

fact n = foldr  (*) 1 (unfold (\a->(a, a-1)) (<1) n)