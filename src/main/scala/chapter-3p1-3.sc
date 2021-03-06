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

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    // case Nil => Nil
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  // exercise 3.3
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    // case Nil => Nil
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  // exercise 3.4
  def drop[A](n: Int, l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,t) => {
      if (n > 0) drop(n-1, t)
      else l
    }
  }

  // Alternative implementation:
  def dropAlternative[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => dropAlternative(t, n-1)
    }

  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if (f(h)) dropWhile(t, f)
      else l
    }
  }

  // More elegant implementation:
  def dropWhileAlternative[A](l: List[A], f: A => Boolean): List[A] =
  l match {
    case Cons(h,t) if f(h) => dropWhileAlternative(t, f)
    case _ => l
  }

  // exercise 3.6
  def init[A](l: List[A]): List[A] =
  l match {
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  // solution with local mutable buffer - still meets RT
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }
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
// corner cases:
// should return Nil
List.drop(4, List(1,2,3))
// should return Nil
List.drop(2, List())



// exercise 3.5
// Implement dropWhile, which removes elements
// from the List prefix as long
// as they match a predicate.

// nothing to drop:
List.dropWhile(List(1,2,3), (i: Int) => false)
// drop all
List.dropWhile(List(1,2,3), (i: Int) => true)
// drop part of the list
List.dropWhile(List(0, 2, 4, 1, 3), (i: Int) => i % 2 == 0)

// exercise 3.6
// Implement a function, init,
// that returns a List consisting of
// all but the last element of a List.
List.init(List(1,2,3,4))
