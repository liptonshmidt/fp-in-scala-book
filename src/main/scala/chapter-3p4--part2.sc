// 3.4.1 More functions for working with lists

sealed trait List[+A]
// data constructors
case object Nil extends List[Nothing]
// Cons -- short for 'construct'
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)  }
}
// ex. 3.16: Write a function
// that transforms a list of integers
// by adding 1 to each element.
def incrementAll(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case Cons(h,t) => Cons(h+1, incrementAll(t))
}

incrementAll(List(1,2,3))

def incrementAll2(l: List[Int]): List[Int] = {
  foldRight(l, Nil: List[Int])((h, t) => Cons(h+1, t))
}

incrementAll2(List(1,2,3))

// ex. 3.17: Write a function that turns each value
// in a List[Double] into a String.
// You can use the expression d.toString
// to convert some d: Double to a String.
def turnDoubleValtoString(l: List[Double]): List[String] = {
  foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
}

turnDoubleValtoString(List(1.0, 2.3, 3.1414))
// ex. 3.18: Map
def map[A,B](as: List[A])(f: A => B): List[B] = {
  foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))
}

map(List(1,2,3))(_ + 1)
map(List(1,2,3))(_.toString)

// implementation via local mutation:
def map_2[A,B](l: List[A])(f: A => B): List[B] = {
  val buf = new collection.mutable.ListBuffer[B]
  def go(l: List[A]): Unit = l match {
    case Nil => ()
    case Cons(h,t) => buf += f(h); go(t)
  }
  go(l)
  List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
}

// Info on Scala _* notation:
// http://stackoverflow.com/questions/1124099/what-is-the-meaning-of-colon-underscore-and-star-in-lifts-sitemapentries

//.. append the array argument with a colon
// and an _* symbol, like this:
// scala> echo(arr: _*)
//This notation tells the compiler to pass
// each element of arr as its own argument to echo,
// rather than all of it as a single argument.

// ex. 3.19:
// Write a function filter that removes elements
// from a list unless they satisfy a given predicate.
// Use it to remove all odd numbers from a List[Int].
def filter[A](as: List[A])(f: A => Boolean): List[A] = {
  val buf = new collection.mutable.ListBuffer[A]
  def go(l: List[A]): Unit = l match {
    case Nil => ()
    case Cons(h,t) => if(f(h)) buf += h; go(t)
  }

  go(as)
  List(buf.toList: _*)
}

filter(List(1,2,3,4,5))(_ % 2 == 0)



def filterViaFoldRight[A](l: List[A])(f: A => Boolean): List[A] = {
   foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h,t) else t)
}

filterViaFoldRight(List(1,2,3,4,5))(_ % 2 == 0)

// ex. 3.20
// Write a function flatMap that works
// like map except that the function given
// will return a list instead of a single result,
// and that list should be inserted into
// the final resulting list.

def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
  flatten(map_2(l)(f))
}


def flatten[A](l: List[List[A]]): List[A] = {
  foldRight(l, Nil:List[A])(appendViaFoldRight)
}

def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
  foldRight(a1, a2)(Cons(_,_))
}

flatMap(List(1,2,3))(i => List(i,i))
// should result in
// List(1,1,2,2,3,3).
