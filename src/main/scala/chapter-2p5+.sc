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

