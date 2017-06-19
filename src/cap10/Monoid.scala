package cap10

/**
  * The MIT License (MIT)
  *
  * Copyright (c) 2015 Riccardo Cardin
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy
  * of this software and associated documentation files (the "Software"), to deal
  * in the Software without restriction, including without limitation the rights
  * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  * copies of the Software, and to permit persons to whom the Software is
  * furnished to do so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in all
  * copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  * SOFTWARE.
  */

/**
  * Solutions to exercises of chapter 10 of the book "Functional Programming in Scala".
  *
  * @author Riccardo Cardin
  * @version 1.0
  * @since 1.0
  */
object Monoid {
  trait Monoid[A] {
    // Satisfies op(op(x, y), z) == op(x, op(y, z))
    def op(a1: A, a2: A): A
    // Satisfies op(x, zero) == op(zero, x) == x
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }

  // Exercise 10.1
  // Give Monoid instances for integer addition, and multiplication as well as the Boolean operators
  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }
}
