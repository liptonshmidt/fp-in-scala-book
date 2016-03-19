// 3.25 Trees
// Tuples
val p = ("Bob", 42)
p._1
p._2
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// ex. 3.25
// Write a function size that counts
// the number of nodes (leaves and branches) in a tree.
def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l,r) => 1 + size(l) + size(r)
}
val l1 = Leaf("test-1")
val l2 = Leaf("test-2")
val b = Branch(l1, l2)
size(b)


// ex. 3.26
// Write a function maximum that returns
// the maximum element in a Tree[Int].
// (Note: In Scala, you can use x.max(y)
// or x max y to compute the maximum
// of two integers x andy.)

def maxEl(t: Tree[Int]): Int = t match {
  case Leaf(x: Int) => x
  case Branch(l, r) => maxEl(l) max maxEl(r)
}
val l11 = Leaf(1)
val l12 = Leaf(2)
val b1 = Branch(l11, l12)
maxEl(b1)

// ex. 3.27
// Write a function depth
// that returns the maximum path length
// from the root of a tree to any leaf.

def depth[A](t: Tree[A]):Int = t match {
  case Leaf(_)  => 0
  case Branch(l, r) => 1 + (depth(l) max depth(r))
}

assert(depth(b) == 1)

// ex. 3.28
// Write a function map,
// analogous to the method
// of the same name on List,
// that modifies each element in a tree
// with a given function.

def mapTree[A](t: Tree[A]): Tree[A] = {
  ???
}
