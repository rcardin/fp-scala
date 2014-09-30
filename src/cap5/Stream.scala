import scala.annotation.tailrec

import Stream._
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

  // Exercise 2
  // Write the function take(n) for returning the first n elements of a Stream,
  // and drop(n) for skipping the first n elements of a Stream.
  def take(n: Int): Stream[A] =
    if (n == 0) Empty
    else
      this match {
        case Cons(h, t) => cons(h(), take(n - 1))
        case _ => Empty
      }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def go(acc: Stream[A], n: Int): Stream[A] =
      if (n == 0) acc
      else
        acc match {
          case Cons(h, t) => go(t(), n - 1)
          case _ => Empty
        }
    go(this, n)
  }


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty
}