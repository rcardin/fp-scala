// import scala.{Option => _, Either => _, _}

sealed trait Either[+E,+A] {
  // Exercise 6
  // Implement version of map, flatMap, orElse and map2 on Either that
  // operate on the Right value
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(_) => this
      case Left(_) => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for { aa <- this; bb <- b } yield f(aa, bb)
    // OR IN OTHER WORDS
    // this flatMap(aa => b map(bb => f(aa, bb)))
}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  // Exercise 7
  // Implement sequence and traverse for Either. These should return
  // the first error that's encountered, if there is one.
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil)){(h, t) => f(h) map2(t)(_ :: _)}

  def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse_1(t)(f))(_ :: _)
    }

  def sequence[E, A](es: List[Either[E, A]]):Either[E, List[A]] = ???

}