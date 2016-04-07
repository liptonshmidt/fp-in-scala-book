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
}

// exercise 4.2
def variance(xs: Seq[Double]): Option[Double] =
  mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

//def mean(xs: List[Double]): Option[Double] = {
//  if (xs.length > 0) {
//    Some[Double](xs.sum / xs.length)
//  } else {
//    None[Double]
//  }
//}

//Some(1)
//val xs = Seq(1.0, 2.0)
//xs.sum
//xs.length
//Some(xs.sum / xs.length)

//val x = List(1.0, 2.0)
//println(x.sum)
//
//println(List(1,3,3).sum)
