import scala.annotation.tailrec

// Exercise 1
// Write a recursive function to get the nth Fibonacci number.
def fib(n: Int): Int = {
  @tailrec
  def go(n: Int, prev: Int, acc: Int): Int =
    if (n == 1) acc
    else go(n - 1, acc, acc + prev)
  go(n, 1, 0)
}
fib(6)
// Exercise 2
// Implement isSorted which checks whether an Array[A] is sorted
// according to a given comparison function.
def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
  @tailrec
  def go(i: Int): Boolean = {
    if (i == as.length) true
    else if (gt(as(i - 1), as(i))) go(i + 1)
    else false
  }
  if (as.isEmpty) false
  else go(1)
}
isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x < y)
// Exercise 3
// Implement curry which converts a function f of two arguments into
// a function of one argument that partially applies f.
def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  a => b => f(a, b)
def c1 = curry[Int, Int, Int]((x, y) => x + y)
def c2 = c1(4)
c2(7)
// Exercise 4
// Implement uncurry, which reverses the transformation of curry
def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)
def c3 = uncurry(c1)
c3(3, 4)
// Exercise 5
// Implement the high-order function compose that composes two functions.
def compose[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))