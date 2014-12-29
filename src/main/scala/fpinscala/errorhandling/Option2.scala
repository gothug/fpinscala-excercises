package fpinscala.errorhandling

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{Some => _, None => _, Option => _, Either => _, _}

sealed trait Option2[+A] {
  def map[B](f: A => B): Option2[B]

  def flatMap[B](f: A => Option2[B]): Option2[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => Option2[B]): Option2[B]

  def filter(f: A => Boolean): Option2[A]
}

case object None2 extends Option2[Nothing] {
  def map[B](f: Nothing => B): Option2[B] = None2

  def flatMap[B](f: Nothing => Option2[B]): Option2[B] = None2

  def getOrElse[B >: Nothing](default: => B): B = default

  def orElse[B >: Nothing](ob: => Option2[B]): Option2[B] = ob

  def filter(f: Nothing => Boolean): Option2[Nothing] = None2
}

case class Some2[+A](get: A) extends Option2[A] {
  def map[B](f: A => B): Option2[B] = Some2(f(get))

  def flatMap[B](f: A => Option2[B]): Option2[B] = f(get)

  def getOrElse[B >: A](default: => B): B = get

  def orElse[B >: A](ob: => Option2[B]): Option2[B] = this

  def filter(f: A => Boolean): Option2[A] = if (f(get)) this else None2
}

object Option2 {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option2[Double] =
    if (xs.isEmpty) None2
    else Some2(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option2[Double] =
    mean(xs).flatMap {
      m =>
        mean(xs.map(x => math.pow(x - m, 2)))
    }

  def map2[A, B, C](a: Option2[A], b: Option2[B])(f: (A, B) => C): Option2[C] = (a, b) match {
    case (Some2(x), Some2(y)) => Some2(f(x, y))
    case _                    => None2
  }

  def map22[A, B, C](a: Option2[A], b: Option2[B])(f: (A, B) => C): Option2[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  def sequence[A](a: List[Option2[A]]): Option2[List[A]] = a match {
    case x :: xs => sequence(xs).flatMap(l => x.map(_ :: l))
    case List()  => Some2(List())
  }

  def traverse[A, B](a: List[A])(f: A => Option2[B]): Option2[List[B]] =
    a match {
      case Nil => Some2(Nil)
      case x :: xs => f(x) flatMap (xx => traverse(xs)(f).map(xx :: _))
    }
}