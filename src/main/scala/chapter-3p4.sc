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
  // more concise notation:
//  foldRight(ds, 1.0)(_ * _)
}

product2(List(2,3,4))

foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

// ex. 3.9
def length[A](l: List[A]): Int = {
  foldRight(l, 0)((_, ac) => ac + 1)
}

length(List(1,3,4))
length(List())

// ex. 3.10
@annotation.tailrec
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)  }
}

// ex. 3.11
// sum, product and List length using foldLeft

//---
def sum3(ints: List[Int]): Int = {
  foldLeft(ints, 0)(_ + _)
}

sum3(List(1,2,5))

//---
def product3(ds: List[Double]): Double = {
  foldLeft(ds, 1.0)(_ * _)
}

product3(List(2,3,5))

//---
def length3[A](l: List[A]): Int = {
  foldLeft(l, 0)((ac, _) => ac + 1)
}

length3(List())
length3(List(1, 2, 4))
