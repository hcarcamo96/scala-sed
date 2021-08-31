val myList = List(1,2,3)

val myMap = Map("e" -> List())

println(4 :: myList)
println(4 :: myMap("e"))

println(myMap ++ Map("f" -> (4 :: List())))