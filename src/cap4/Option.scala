sealed trait Option[+A] {
  // Exercise 1
  // Implement the following function on Option
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map(Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 2
  // Implement the variance function in terms of flatMap
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // Exercise 3
  // Write a generic function map2 that combines two Option values
  // using a binary function.
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

//    (a, b) match {
//      case (None, _) => None
//      case (_, None) => None
//      case (Some(as), Some(bs)) => Some(f(as, bs))
//    }

  // Exercise 4
  // Write a function that combines a list of Options into one Option containing a
  // list of all the Some values in the original list. If the original list contains
  // None even once, the result of the function should be None.
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case x :: xs => x flatMap(xx => sequence(xs) map(xx :: _))
    }

  // Exercise 5
  // Implement traverse.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a.map(f))

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case x :: xs => f(x) flatMap(xx => traverse_1(xs)(f) map(xx :: _))
    }

  def traverse_2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse_2(t)(f))(_ :: _)
    }
}