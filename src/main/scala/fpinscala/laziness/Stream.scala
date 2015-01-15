package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => Cons(h, () => Empty)
    case Cons(h, t) if n >= 1 => Cons(h, () => t().take(n - 1))
    case _                    => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n == 0 => this
    case Cons(h, t) if n > 0  => t().drop(n - 1)
    case Empty                => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      if (p(h())) Cons(h, () => t() takeWhile p)
      else Empty
    case Empty => Empty
  }

  def map[B](p: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => Cons(() => p(h), () => t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](p: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => p(h) append t)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
    case Empty      => true
  }

  def forAllViaFoldRight(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) => if (p(h)) Cons(() => h, () => t) else Empty)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList: List[A] = {
    @annotation.tailrec
    def loop(acc: List[A], stream: Stream[A]): List[A] = stream match {
      case Empty => acc
      case Cons(h, t) => loop(h() :: acc, t())
    }

    loop(List(), this).reverse
  }
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

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}

object Test {
  def main(args: Array[String]): Unit = {
    println("\n")
    println("toList:")
    println("---------")

    printlnBulk(
      Stream(1 to 100),
      Stream(),
      Stream(1 to 100: _*).toList,
      Stream(1 to 1000000: _*).toList.length,
      Stream().toList
    )

    println("\n")
    println("take:")
    println("---------")

    printlnBulk(
      Stream(1 to 10: _*).take(5).toList,
      Stream().take(5).toList
    )

    printlnBulk(
      "\n",
      "drop:",
      "-----",
      Stream(1 to 10: _*).drop(3).toList,
      Stream(1 to 10: _*).drop(100).toList
    )

    printlnBulk(
      "\n",
      "takeWhile:",
      "----------",
      Stream(1, 2, 3, 4).takeWhile(_ > 0).toList,
      Stream(1, 2, -3, 4).takeWhile(_ > 0).toList,
      Stream(-1, 2, -3, 4).takeWhile(_ > 0).toList,
      (Stream(): Stream[Int]).takeWhile(_ > 0).toList
    )

    printlnBulk(
      "\n",
      "forAllViaFoldRight:",
      "-------------------",
      Stream(1, 2, 3, -4).forAllViaFoldRight(_ > 0),
      Stream(1, 2, 3).forAllViaFoldRight(_ > 0)
    )

    printlnBulk(
      "\n",
      "takeWhileViaFoldRight:",
      "----------------------",
      Stream(1, 2, 3, 4).takeWhile(_ > 0).toList,
      Stream(1, 2, -3, 4).takeWhile(_ > 0).toList,
      Stream(-1, 2, -3, 4).takeWhile(_ > 0).toList,
      (Stream(): Stream[Int]).takeWhile(_ > 0).toList
    )

    printlnBulk(
      "\n",
      "map:",
      "----",
      Stream(1, 2, 3, 4).map(_ * 2).toList
    )

    printlnBulk(
      "\n",
      "filter:",
      "----",
      Stream(-1, 2, 3, -4).filter(_ > 0).toList,
      Stream(-1, -4).filter(_ > 0).toList
    )

    printlnBulk(
      "\n",
      "flatMap:",
      "----",
      Stream(1, 2, 3, 4).flatMap(x => Stream(x, x)).toList
    )
  }

  def printlnBulk[T](ss: T*): Unit =
    for (s <- ss)
      println(s)
}
