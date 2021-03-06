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

// ex. 3.12
// Write a function that returns
// the reverse of a list
// (given List(1,2,3) it returns List(3,2,1)).
// See if you can write it using a fold.
def reverse[A](l: List[A]): List[A] = {
  foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
}

reverse(List(1,2,3))
reverse(List())

// ex 3.13
// Hard: Can you write foldLeft in terms of foldRight?
// How about the other way around?
// Implementing foldRight via foldLeft is useful
// because it lets us implement foldRight tail-recursively,
// which means it works even for large lists
// without overflowing the stack.

def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
  foldLeft(reverse(l), z)((b,a) => f(a,b))

def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
  foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

// ex. 3.14
// Implement append in terms of
// either foldLeft or foldRight.

//def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
//  case Nil => a2
//  case Cons(h,t) => Cons(h, append(t, a2)) }

def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
  foldRight(a1, a2)(Cons(_,_))
}

appendViaFoldRight(List(1,2,3), List(4,5,6))

// ex. 3.15
// Hard: Write a function that concatenates
// a list of lists into a single list.
// Its runtime should be linear
// in the total length of all lists.
// Try to use functions we have already defined.

def flatten[A](l: List[List[A]]): List[A] = {
  foldRight(l, Nil:List[A])(appendViaFoldRight)
}

flatten(List(List(1,2,3), List(4), List(5,6)))
