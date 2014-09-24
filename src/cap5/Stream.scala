import scala.annotation.tailrec

sealed trait Stream[+A] {
  // Esercise 1
  // Write a function to convert a Stream to a List, which will force
  // its evaluation and let you look at it in the REPL
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def toListTail: List[A] = {
    @tailrec
    def go(acc: List[A], s: Stream[A]): List[A] =
      s match {
        case Empty => acc
        case Cons(h, t) => go(h() :: acc, t())
      }
    go(List(), this).reverse
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]