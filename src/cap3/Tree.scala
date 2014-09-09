sealed trait Tree[+A]
case class Leaf[A](value: A) extends  Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 25
  // Write a function size that counts the number of nodes in a tree.
  def size[A](root: Tree[A]): Int =
    root match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  // Exercise 26
  // Write a function maximum that return the maximum element in a Tree[Int]
  def maximum(root: Tree[Int]): Int =
    root match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  // Exercise 27
  // Write a function depth that returns the maximum path length from the
  // root of a tree to any leaf
  def depth[A](root: Tree[A]): Int =
    root match {
      case Leaf(_) => 0
      case Branch(l, r) => (depth(l) max depth(r)) + 1
    }

  // Exercise 28
  // Write a function map, that modifies each element in a tree with
  // a given function
  def map[A, B](root: Tree[A])(f: A => B): Tree[B] =
    root match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  // Exercise 29
  // TODO
}