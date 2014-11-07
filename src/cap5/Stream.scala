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

  // Exercise 3
  // Write the function takeWhile for returning all starting elements of a
  // Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile(p))
      case _ => Empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // Exercise 4
  // Implement forAll, which checks that all elements in the Stream match a
  // given predicate. Your implementation should terminate the traversal as
  // soon as it encounters a nonmatching value.
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)
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