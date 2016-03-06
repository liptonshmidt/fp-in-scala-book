// 2.5: Polymorphic functions: abstracting over types
def findFirst(ss: Array[String], key: String): Int = {
  @annotation.tailrec
  def loop(i: Int): Int = {
    if (i >= ss.length) -1
    else if (ss(i) == key) i
    else loop(i+1)
  }

  loop(0)
}

findFirst(Array("Hi", "Brother", "Happy", "Returns"), "Happy")
findFirst(Array("Hi", "Brother", "Happy", "Returns"), "Trains")

def polymorphicFindFirst[A](ss: Array[A], p: A => Boolean): Int = {
  @annotation.tailrec
  def loop(i: Int): Int = {
    if (i >= ss.length) -1
    else if ( p(ss(i)) ) i
    else loop(i+1)
  }

  loop(0)
}

def checkF_1[A](a: A): Boolean = {
   a == "Happy"
}

def checkF_2[A](a: A): Boolean = {
  a == "Trains"
}

polymorphicFindFirst(Array("Hi", "Brother", "Happy", "Returns"), checkF_1)
polymorphicFindFirst(Array("Hi", "Brother", "Happy", "Returns"), checkF_2)

// ---
// Exercise 2.2

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(i: Int): Boolean = {
    print(i)
    print(as.length)
    if (i+1 >= as.length) true
    else if (!ordered(as(i), as(i+1))) false
    else go(i+1)
  }

  go(0)
}

val sortedArr = Array(1, 2, 2, 3)
val notSortedArr = Array(3, 4, 1, 2)
def isOrderedInt(x1: Int, x2: Int): Boolean = {
  x1 <= x2
}

isSorted(sortedArr, isOrderedInt)
isSorted(notSortedArr, isOrderedInt)
