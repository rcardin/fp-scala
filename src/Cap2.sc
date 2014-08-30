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