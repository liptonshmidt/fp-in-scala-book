//case class Some[+A](get: A) extends Option[A]
//case object None extends Option[Nothing]

sealed trait Option[+A] {

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A] (default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap__withoutPM[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None


  def orElse__withPM[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap((a) => if(f(a)) Some(a) else None)

  // lift function
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
}

//case class Some[+A](get: A) extends Option[A]
//case object None extends Option[Nothing]

object Chapter04 {
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  // exercise 4.2 (implement variance)
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // exercise 4.3
  // Write a generic function map2 that combines
  // two Option values using a binary function.
  // If either Option value is None,
  // then the return value is too.
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x,y))
      case _ => None
    }

  // exercise 4.4
  // Write a function sequence that combines a list of Options
  // into one Option containing a list
  // of all the Some values in the original list.
  // If the original list contains None even once,
  // the result of the function should be None;
  // otherwise the result should be Some
  // with a list of all the values.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h::t => h flatMap (hh => sequence(t) map (hh :: _))
  }
}


