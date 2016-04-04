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

  //  def getOrElse[B >: A] (default: => B): B
  //  def orElse[B >: A](ob: => Option[B]): Option[B]
  //  def filter(f: A => Boolean): Option[A]
}

