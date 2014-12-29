package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e)  => Left(e): Either[E, B]
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e)  => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(_) => this
      case Left(_)  => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for { aa <- this; bb <- b } yield f(aa,bb)
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sys.error("todo")

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = sys.error("todo")

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
}

object Test {
  def main(args: Array[String]): Unit = {
    // map
    println(
      Right(4).map(_ * 2)
    )

    println(
      Left("Some error").map((x: Int) => x * 2)
    )

    // flatMap
    val flatMapFun = (x: Int) => if (x > 0) Right(x * 2) else Left("error")

    println {
      Right(4).flatMap(flatMapFun)
    }

    println {
      Right(0).flatMap(flatMapFun)
    }

    println("\n")
    println("orElse:")
    println("-------")

    println {
      Right(0).orElse(Left("error of second Either"))
    }

    println {
      Left("error of first Either").orElse(Left("error of second Either"))
    }

    println("\n")
    println("map2:")
    println("-----")

    printlnBulk(
      Right(4).map2(Right(5))((a: Int, b: Int) => a * b),
      Left("foo").map2(Right(5))((a: Int, b: Int) => a * b),
      Right(4).map2(Left("foo"))((a: Int, b: Int) => a * b),
      Left("foo").map2(Left("foo"))((a: Int, b: Int) => a * b)
    )
  }

  def printlnBulk[T](ss: T*): Unit =
    for (s <- ss)
      println(s)
}