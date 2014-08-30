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