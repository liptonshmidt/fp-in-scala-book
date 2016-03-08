// Adding sealed in front means that
// all implementations of the trait
// must be declared in this file.
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
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    // case Nil => Nil
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](h: A, l: List[A]): List[A] = l match {
    // case Nil => Nil
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](n: Int, l: List[A]): List[A] = {
    ???
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("Star", Cons("Wars", Nil))

// 3.2 Pattern matching

List(1, 2, 3) match {
  case _ => 42
}

List(1,2,3) match { case Cons(h,_) => h }
List(1,2,3) match { case Cons(_,t) => t }
// returns MatchError in runtime
//List(1,2,3) match { case Nil => 42 }


// exercise 3.1

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  // first match
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

// exercise 3.2
// tail function
List.tail(List(1,2,3))

// exercise 3.3
List.setHead(0, List(1,2,3))

// exercise 3.4
List.drop(0, List(1,2,3))
List.drop(1, List(1,2,3))
List.drop(2, List(1,2,3))
List.drop(3, List(1,2,3))
List.drop(4, List(1,2,3))
