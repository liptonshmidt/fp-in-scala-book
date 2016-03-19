// 3.5 Trees

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

