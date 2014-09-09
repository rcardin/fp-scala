package cap3

import annotation.tailrec
/**
 * Created by famiglia on 09/09/2014.
 */
sealed trait List[+A]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]
// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

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

//  def append[A](a1: List[A], a2: List[A]): List[A] =
//    a1 match {
//      case Nil => a2
//      case Cons(h,t) => Cons(h, append(t, a2))
//    }

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

  // Exercise 13
  // Can you write foldLeft in terms of foldRight? How about the
  // other way around?
  // TODO

  // Exercise 14
  // Implement append in terms of either foldLeft or foldRight
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  // Exercise 15
  // TODO

  // Exercise 16
  // Write a function that transforms a list of integers by adding 1 to each element.
  def plusOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1 , t))

  // Exercise 17
  // Write a function that turns each value in a List[Double] into a String.
  def toString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  // Exercise 18
  // Write a function map that generalizes modifying each element in a list
  // while maintaining the structure of the list.
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  // Exercise 19
  // Write a function filter that removes elements from a list unless they satisfy
  // a given predicate
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  // Exercise 20
  // Write a function flatMap that works like map except that the function given will
  // return a list instead of a single result, and list should be inserted into the
  // final resulting list.
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(map(l)(f), Nil: List[B])(append)

  // Exercise 21
  // Use flatMap to implement filter
  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(h => if (f(h)) List(h) else Nil)

  // Exercise 22
  // Write a function that accepts two lists and constructs a new list by
  // adding corresponding elements.
  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

  // Exercise 23
  // Generalize the function you just wrote so that it's not specific
  // to integers or addition.
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
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

    println(plusOne(l))

    println(flatMap(List(1, 2, 3))(i => List(i, i)))

    println(filter2(l)(h => h % 2 == 0))
  }
}