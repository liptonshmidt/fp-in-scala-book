val test = 1

//1.3
//Referential transparency, purity, and the substitution model

val x = "Hello, World!"

val r1 = x.reverse
val r2 = x.reverse

//

val sb_x = new StringBuilder("Hello")
val sb_y = sb_x.append(", World")

val t1 = sb_x.reverse
val t2 = sb_y.reverse
//t1 and t2 are the same

val z = new StringBuilder("Hello")
val z1 = z.append(", World").toString
val z2 = z.append(", World").toString
//--> StringBuilder#append is not pure function
