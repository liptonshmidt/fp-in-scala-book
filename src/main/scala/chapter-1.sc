val test = 1

//1.3
//Referential transparency, purity, and the substitution model

val x = "Hello, World!"

val r1 = x.reverse
val r2 = x.reverse

val sb_x = new StringBuilder("Hello")
val sb_y = sb_x.append(", World")

