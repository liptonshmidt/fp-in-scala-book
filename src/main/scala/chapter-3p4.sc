// 3.4
// Recursion over lists
// and
// generalizing to higher-order functions

sealed trait List[+A]

// data constructors
case object Nil extends List[Nothing]
// Cons -- short for 'construct'
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

//List(1,2,3).sum
List.sum(List(1,2,3))

// #product and #similar have similar structure

//Again, placing f in its own argument group
// after as and z lets type inference
// determine the input types to f.
def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def sum2(ints: List[Int]): Int = {
  foldRight(ints, 0)((x, y) => x + y)
}

sum2(List(1,2,3))

def product2(ds: List[Double]): Double = {
  foldRight(ds, 1.0)((x,y) => x * y)
}

product2(List(2,3,4))

