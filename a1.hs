-- a person is Male or Female, Abdicated, Dead or Alive, and has a  name
data Sex = Male | Female deriving (Eq, Show)
data Status = Alive | Dead | Abdicated deriving (Eq, Show)
data Person = Person Sex Status String deriving (Eq, Show)

-- a Dynasty is headed by a Person and indicates the descendants
-- oldest first; a Dull Person doesn't have any recorded descendants
data Dynasty = Descend Person [Dynasty] | Dull Person deriving (Eq, Show)

-- |find successors after a person named N in some dynasty
successors :: String -> Dynasty -> [String]
successors name dynasty =
    aliveafter name (linefrom dynasty)

-- |return the list of all Persons in order of succession in Dynasty dy
linefrom :: Dynasty -> [Person]
linefrom dy = 
    case dy of
        (Dull p) -> [p]
        (Descend p dys) -> let newdys = removeAbdicated (dynastyDescend (reorder (Descend p dys))) in  --first reorder and remove abdicated dynasties
                                          foldl (++) [p] (map linefrom newdys) -- then get all the lines recursively and concatenate them 

-- |remove dynastys of people who have abdicated from a dynasty list
removeAbdicated::[Dynasty]->[Dynasty]
removeAbdicated dys = 
    case dys of
        [] -> []
        (x:xs)-> if personStatus (dynastyPerson x) == Abdicated  --If person's status is abdicated, remove it from the list
                        then removeAbdicated xs
                        else x:(removeAbdicated xs)

-- |sort dys so that males precede females, not changing the order within each sex
reorder :: Dynasty -> Dynasty
reorder dy =
    case dy of
        (Dull p) -> (Dull p)
        (Descend p dys) -> Descend p $sortds dys

-- |  sort dys so that males precede females, not changing the order within each sex
sortds :: [Dynasty] -> [Dynasty]
sortds dys = foldr insertd [] dys

-- |insert dy into sorted position in dys (see sortds above)
insertd :: Dynasty -> [Dynasty] -> [Dynasty]
insertd dy dys =
    case dys of
        [] -> [dy] -- return a list of one element
        (x:xs) ->  if (personSex (dynastyPerson dy)) == Male -- If element to insert if male, insert it to the head of list
                            then (dy:x:xs)
                            else if (personSex (dynastyPerson x )) == Female -- Or if fisrt element is female, insert it to the head of list 
                                        then (dy:x:xs)
                                        else x: (insertd dy xs) -- If not, do insertd recursively

-- | the names of the Alive persons in ps occurring after name
aliveafter :: String -> [Person] -> [String]
aliveafter name ps=
    case ps of
        []->[]
        (x:xs)->alivein (nameafter name (x:xs))

-- | return a list of names after a specified name
nameafter::String->[Person]->[Person]
nameafter name ps = 
    case ps of
        []->[]
        (x:xs)->    if  personString x == name 
                            then xs -- If we find the name, return the rest of the list
                            else nameafter name xs -- Otherwise keeping searching recursively

-- | the names of the Alive persons in ps
alivein :: [Person] -> [String]
alivein ps =
    case ps of
        [] -> []
        (x:xs) -> if (personStatus x) == Alive  
                            then (personString x):(alivein xs)  -- If a person is alive ,put  his name into the list
                            else alivein xs 

-- accessing function for elements of Person
personSex (Person sex sta str) =  sex

personStatus (Person sex sta str) =  sta

personString (Person sex sta str) =  str

-- accessing function for elements of Dynasty
dynastyPerson (Descend p dys) = p

dynastyPerson (Dull p) = p

dynastyDescend (Descend p dys) = dys

-- example dynasty
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
