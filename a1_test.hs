george5 = Person Male Dead "George5"
edward8 =  Person Male Abdicated "Edward8"
george6 = Person Male Dead "George6"
mary = Person Female Dead "Mary"
henry = Person Male Dead "Henry"
george1 = Person Male Dead "George"
john = Person Male Dead "John"
elizabeth2 = Person Female Alive "Elizabeth2"
margaret = Person Female Dead "Margaret"
charles = Person Male Alive "Charles"
anne = Person Female Alive "Anne"
andrew = Person Male Alive "Andrew"
edward = Person Male Alive "Edward"
william = Person Male Alive "William"
george2 = Person Male Alive "George"
harry = Person Male Alive "Harry"
peter = Person Male Alive "Peter"
savannah = Person Female Alive "Savannah"
isla = Person Female Alive "Isla"
zarah = Person Female Alive "Zarah"
beatrice = Person Female Alive "Beatrice"
eugenie = Person Female Alive "Eugenie"
louise = Person Female Alive "Louise"
david = Person Male Alive "David"
sarah = Person Female Alive "Sarah"
james = Person Male Alive "James"
dynasty26 = Dull isla
dynasty25 = Dull savannah
dynasty24 = Dull george2
dynasty23 = Dull james
dynasty22 = Dull louise
dynasty21 = Dull eugenie
dynasty20 = Dull beatrice
dynasty19 = Dull zarah
dynasty18 = Descend peter [dynasty25, dynasty26]
dynasty17 = Dull harry
dynasty16 = Descend william [dynasty24]
dynasty15 = Dull sarah
dynasty14 = Dull david
dynasty13 = Descend edward [dynasty22, dynasty23]
dynasty12 = Descend andrew [dynasty20, dynasty21]
dynasty11 = Descend anne [dynasty18, dynasty19]
dynasty10 = Descend charles [dynasty16, dynasty17]
dynasty9  = Descend margaret [dynasty14, dynasty15]
dynasty8  = Descend elizabeth2 [dynasty10, dynasty11, dynasty12, dynasty13]
dynasty7  = Dull mary
dynasty6  = Dull john
dynasty5  = Dull george1
dynasty4  = Dull henry
dynasty3  = Descend george6 [dynasty8, dynasty9]
dynasty2  = Dull edward8
dynasty1  = Descend george5 [dynasty2,dynasty3,dynasty4,dynasty5,dynasty6,dynasty7]

ai_test1 = 
    if (["William","Peter","Harry"] == alivein [william, mary, john, peter, harry])
        then "OK" 
    else "fail"

ai_test2 = 
    if ([] == alivein [mary, john])
        then "OK" 
    else "fail"

af_test1 = 
    if (["Peter","Harry"] == aliveafter "Mary" [william, mary, john, peter, harry])
        then "OK" 
    else "fail"

af_test2 = 
    if ([] == aliveafter "Edward8" [william, mary, john, peter, harry])
        then "OK" 
    else "fail"

af_test3 = 
    if ([] == aliveafter "Harry" [william, mary, john, peter, harry])
        then "OK" 
    else "fail"

af_test4 = 
    if ([] == aliveafter "Harry" [])
        then "OK" 
    else "fail"


exdyn1 =
    Descend (Person Male Dead "George5")
    [
        Descend (Person Male Alive "Edward8") [],
        Descend (Person Male Abdicated "George6")
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

                    Descend (Person Female Alive "Peter")

                    [

                        Dull (Person Male Alive "Savannah"),

                        Dull (Person Female Alive "Lily"),
                        Dull (Person Female Alive "Tera"),
                        Dull (Person Male Alive "Bob")

                    ],

                    Dull (Person Male Alive "Zarah")

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

ab_test = if (["Edward8"] == successors "George5" exdyn1) then "OK" else "fail"
ab_test1 = if ([] == successors "Anne" exdyn1) then "OK" else "fail"

main = putStrLn ("alivein: " ++ ai_test1 ++ " " ++ ai_test2 ++ "\n" ++ 
    "aliveafter: " ++ af_test1 ++ " " ++ af_test2 ++ " " ++ af_test3 ++ " " ++ af_test4 ++ "\n" ++
    "Abdicated_test: " ++ ab_test ++ " " ++ ab_test1 ++ "\n")