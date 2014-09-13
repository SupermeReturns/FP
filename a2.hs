{--
Copyright (C) 1998 - 2011
You may opt to use, copy, modify, merge, publish, distribute and/or sell
copies of the Software, and permit persons to whom the Software is
furnished to do so, under the terms of the COPYING file.

This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
KIND, either express or implied.

Student No.12330284
Student Name:SunDongliang
Email:im123im123@163.com
--}

-- a person is Male or Female, Abdicated, Dead or Alive, and has a  name
data Sex = Male | Female deriving (Eq, Show)
instance Ord Sex where
    compare Male Male = EQ
    compare Male Female = LT
    compare Female Male = GT
    compare Female Female = EQ
data Status = Alive | Dead | Abdicated deriving (Eq, Show)

data Person = Person Sex Status String deriving (Eq, Show)
instance Ord Person where
    compare (Person sx1 _ _) (Person sx2 _ _) = compare sx1 sx2

-- a Dynasty is headed by a Person and indicates the descendants
-- oldest first; a Dull Person doesn't have any recorded descendants
data Dynasty = Descend Person [Dynasty] | Dull Person deriving (Eq, Show)
instance Ord Dynasty where
    compare (Descend p1 _) (Descend p2 _) = compare p1 p2
    compare (Descend p1 _) (Dull p2 ) = compare p1 p2
    compare (Dull p1 ) (Descend p2 _) = compare p1 p2
    compare (Dull p1) (Dull p2 ) = compare p1 p2

-- |find successors after a person named N in some dynasty
successors :: String -> Dynasty -> [String]
successors name dynasty = aliveafter name (linefrom dynasty)

-- define the catamorphism cataD on Dynasty
cataD :: (Person -> [b] -> b) -> (Person -> b) -> Dynasty -> b
cataD ns es (Dull p) = es p
cataD ns es (Descend p dys) = ns p (map (\dy -> cataD ns es dy) dys)

-- then reimplement linefrom to use cataD instead of explicit
-- recursion and the new version of reorder below
linefrom :: Dynasty -> [Person]
linefrom dy = cataD nss ess (reorder dy)
                        where
                        --  non-empty server for linefrom
                        nss p rs = 
                            case p of
                                (Person _ Abdicated _) -> []
                                _ -> p:concat rs
                        -- empty server for linefrom
                        ess p = 
                            case p of
                                (Person _ Abdicated _) -> []
                                _ -> [p]

-- redefine reorder so that all sub-dynasties are sorted
-- with Males before Females, using cataD
reorder :: Dynasty -> Dynasty
reorder dy = cataD (\p rs->Descend p (sortds rs)) (\p -> Dull p) dy

-- reimplement sortds to use new insertd and flatten below
sortds :: [Dynasty] -> [Dynasty]
sortds dys = flatten (foldr insertd Dnull dys)
-- sortds first insert all dynasties into a sorted binary tree,
-- then use flaten to get the sorted list  of dynasties from the tree

-- define a type of binary trees for Dynasty
data BTD = Dnode BTD Dynasty BTD | Dnull

-- define the catamorphism cataBTD on the above type
cataBTD :: (t -> Dynasty -> t -> t) -> t -> BTD -> t
cataBTD ns es Dnull = es
cataBTD ns es (Dnode b1 dy b2) = ns (cataBTD ns es b1) dy (cataBTD ns es b2)

-- use cataBTD to define a function to “flatten” btd :: BTD
-- in an in-order traversal
flatten :: BTD -> [Dynasty]
flatten btd = cataBTD (\r1 dy r2 -> r1 ++ [dy] ++ r2) [] btd

-- redefine insertd so that “flatten d” yields every top-level
-- Dynasty in d headed by a Male before every top-level Dynasty
-- in d headed by a Female, in particular so that
-- “flatten(insertd d btd)” yields d before every top-level Dynasty
-- headed by a Person of the same Sex as d
-- insertd :: Dynasty -> [Dynasty] -> [Dynasty]
-- insertd dy [] = [dy]
-- insertd dy (d:ds) = if dy > d then d:(insertd dy ds) else dy:d:ds
insertd :: Dynasty -> BTD -> BTD
insertd dy btd =
    case btd of
        Dnull -> Dnode Dnull dy Dnull
        Dnode b1 dy2 b2->if dy > dy2 -- if dy> dy2 then insert dy into the right branch of btd, else insert dy into the left branch of btd
                                          then Dnode b1 dy2  (insertd dy b2)
                                          else  Dnode (insertd dy b1) dy2 b2

aliveafter :: String -> [Person] -> [String]
aliveafter name ps =
    let fromnam = dropWhile (\(Person _ _ pname)-> name /= pname) ps
    in if null fromnam then [] else alivein (tail fromnam)
    
alivein :: [Person] -> [String]
alivein ps =
    map
        (\(Person _ _ name) -> name)
        (filter (\(Person _ st _) -> st == Alive) ps)

-- example dynasty
exdyn =
    Descend (Person Male Dead "George5")
    [
        Descend (Person Male Abdicated "Edward8") [],
        Descend (Person Male Dead "George6")
        [
            Descend (Person Female Alive "Elizabeth2")
            [
                Descend (Person Male Alive "Charles")
                [
                    Descend (Person Male Alive "William")
                    [
                        Descend (Person Male Alive "George") []
                    ],
                    Descend (Person Male Alive "Harry") []
                ],
                Descend (Person Female Alive "Anne")
                [
                    Descend (Person Male Alive "Peter")
                    [
                        Dull (Person Female Alive "Savannah"),
                        Dull (Person Female Alive "Isla")
                    ],
                    Dull (Person Female Alive "Zarah")
                ],
                Descend (Person Male Alive "Andrew")
                [
                    Dull (Person Female Alive "Beatrice"),
                    Dull (Person Female Alive "Eugenie")
                ],
                Descend (Person Male Alive "Edward")
                [
                    Dull (Person Female Alive "Louise"),
                    Dull (Person Male Alive "James")
                ]
            ],
            Descend (Person Female Dead "Margaret")
            [
                Dull (Person Male Alive "David"),
                Dull (Person Female Alive "Sarah")
            ]
        ],
        Dull (Person Female Dead "Mary"),
        Dull (Person Male Dead "Henry"),
        Dull (Person Male Dead "George"),
        Dull (Person Male Dead "John")
    ]