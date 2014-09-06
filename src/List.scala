import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2
  // Implement a function tail for removing the first element of a List
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new IllegalArgumentException("Empty list")
      case Cons(_, xs) => xs
    }

  // Exercise 3
  // Implement the function setHead for replacing the first element of a List
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => throw new IllegalArgumentException("Empty list")
      case Cons(_, xs) => Cons(h, xs)
    }

  // Exercise 4
  // Implement the function drop, which removes the first n elements of a List
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  // Exercise 5
  // Implement dropWhile, which removes elements from the List prefix as long as
  // they match a predicate
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  // Exercise 6
  // Implement a function, init, that returns a List consisting of all but the
  // last element of a List
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 7
  // Can product immediately halt the recursion and return 0.0 if it encounters a 0.0?
  // No, because of early the evaluation of foldRight arguments

  // Exercise 8
  // See what happens when you pass Nil and Cons themselves to foldRight.
  // It's the identity function

  // Exercise 9
  // Compute the length of a list using foldRight
  def length[A](l: List[A]): Int =
    foldRight(l, 0){ (x, y) => y + 1 }

  // Exercise 10
  // Write another general list-recursion function, foldLeft
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B) : B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // Exercise 11
  // Write sum, product and a function to compute the length of a list
  // using foldLeft
  def sum2(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product2(l: List[Double]) = foldLeft(l, 1d)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0){(x, _) => x + 1}

  // Exercise 12
  // Write a function that returns the reverse of a list
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))
}

object ListMain {
  import List._

  def main(args: Array[String]): Unit = {

    // Exercise 1
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)

    val l = List(1, 2, 3, 4, 5)
    println(drop(l, 2))
    println(init(l))

    // Exercise 8
    println(foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)))
    println(length(l))

    println(reverse(l))
  }
}