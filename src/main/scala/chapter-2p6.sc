// 2.6 Following types to implementations

def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
  (b: B) => f(a, b)


// we can omit the type annotation
def partial1_alt[A,B,C](a: A, f: (A,B) => C): B => C =
  b => f(a, b)


// exercise 2.3
// Currying

def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A)  => b => f(a, b)


def f = (ar: Array[String], ind: Int) => ar(ind)
f(Array("Welcome", "To", "The", "Jungle"), 2)

val curriedF = curry(f)
curriedF(Array("Welcome", "To", "The", "Jungle"))(2)



