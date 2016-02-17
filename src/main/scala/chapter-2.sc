object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}

// Aside from some technicalities, every value in Scala is what’s called an object,
// and each object may have zero or more members.

// An object whose primary purpose is giving its members a namespace
// is sometimes called a module.

// A member can be a method declared with the def keyword,
// or it can be another object declared with val or object.
