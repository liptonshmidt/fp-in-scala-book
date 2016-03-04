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

MyModule.main(Array())

// Aside from some technicalities, every value in Scala is what’s called an object,
// and each object may have zero or more members.

// An object whose primary purpose is giving its members a namespace
// is sometimes called a module.

// A member can be a method declared with the def keyword,
// or it can be another object declared with val or object.

//--- 2.4 ---

def factorial(n: Int): Int =  {
  @annotation.tailrec
  def go(n: Int, acc: Int): Int = {
    if (n <= 0) acc
    else go(n-1, acc*n)
  }

  go(n,1)
}

factorial(3)


// Exercise 2.1

def fib(n: Int): Int = {
  @annotation.tailrec
  def loop(n: Int, prev: Int, cur: Int): Int = {
    if (n == 0) prev
    else loop(n - 1, cur, prev + cur)
  }

  loop(n, 0, 1)
}

fib(1)
fib(2)
fib(3)
fib(4)
fib(5)


// ---
object MyModuleUpdated {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int =  {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, acc*n)
    }

    go(n,1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(x: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(3))
  }
}

MyModuleUpdated.main(Array())


object MyModuleRefactored {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int =  {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, acc*n)
    }

    go(n,1)
  }

  private  def fomatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(x: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(3))
  }
}

MyModuleRefactored.main(Array())
