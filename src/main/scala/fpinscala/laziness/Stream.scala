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

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), x) if x > 0 => Some(h(), (t(), x - 1))
      case _                        => None
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

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _                    => None
    }

  def map[B](p: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => Cons(() => p(h), () => t))

  def mapViaUnfold[B](p: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(p(h()), t())
      case Empty      => None
    }

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

  def zipWithViaUnfold[B, C](s: Stream[B])(p: (A, B) => C): Stream[C] =
    unfold(this, s) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(p(h1(), h2()), (t1(), t2()))
      case _                            => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, s) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2))        => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty)        => Some((Some(h1()), None), (t1(), Empty))
      case _                            => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAll(s).takeWhile(_._2.nonEmpty) forAll {
      case (v1, v2) => v1 == v2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some((Cons(() => h(), () => t()), t()))
      case _          => None
    } append Stream(empty)

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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  def unfoldList[A, S](z: S)(f: S => Option[(A, S)]): List[A] =
    f(z) match {
      case Some((a, s)) => a :: unfoldList(s)(f)
      case None => Nil: List[A]
    }

  def constant[A](a: A): Stream[A] = {
    lazy val s: Stream[A] = cons(a, s)
    s
  }

  def from(n: Int): Stream[Int] =
   cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fib(a: Int, b: Int): Stream[Int] = {
      cons(a, fib(b, a + b))
    }

    fib(0, 1)
  }

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) {case (v0, v1) => Some((v0, (v1, v0 + v1)))}

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n) {x => Some((x, (x + 1)))}

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a) {_ => Some((a, a))}

  def constantLimitedListViaUnfold(a: Int, lim: Int): List[Int] =
    unfoldList((a, 0)) {case (n, cnt) => if (cnt < lim) Some(n, (n, cnt + 1)) else None}

  def onesViaUnfold: Stream[Int] =
    constantViaUnfold(1)
}

object Test {
  def main(args: Array[String]): Unit = {
    println("\n")
    println("toList:")

    printlnBulk(
      Stream(1 to 100),
      Stream(),
      Stream(1 to 100: _*).toList,
      Stream(1 to 1000000: _*).toList.length,
      Stream().toList
    )

    println("\n")
    println("take:")
    printlnBulk(
      Stream(1 to 10: _*).take(5).toList,
      Stream(1 to 5: _*).take(10).toList,
      Stream().take(5).toList
    )

    printlnBulk(
      "\n",
      "takeViaUnfold:",
      Stream(1 to 10: _*).takeViaUnfold(5).toList,
      Stream(1 to 5: _*).takeViaUnfold(10).toList,
      Stream().takeViaUnfold(5).toList
    )

    printlnBulk(
      "\n",
      "drop:",
      Stream(1 to 10: _*).drop(3).toList,
      Stream(1 to 10: _*).drop(100).toList
    )

    printlnBulk(
      "\n",
      "takeWhile:",
      Stream(1, 2, 3, 4).takeWhile(_ > 0).toList,
      Stream(1, 2, -3, 4).takeWhile(_ > 0).toList,
      Stream(-1, 2, -3, 4).takeWhile(_ > 0).toList,
      (Stream(): Stream[Int]).takeWhile(_ > 0).toList
    )

    printlnBulk(
      "\n",
      "forAllViaFoldRight:",
      Stream(1, 2, 3, -4).forAllViaFoldRight(_ > 0),
      Stream(1, 2, 3).forAllViaFoldRight(_ > 0)
    )

    printlnBulk(
      "\n",
      "takeWhileViaFoldRight:",
      Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ > 0).toList,
      Stream(1, 2, -3, 4).takeWhileViaFoldRight(_ > 0).toList,
      Stream(-1, 2, -3, 4).takeWhileViaFoldRight(_ > 0).toList,
      (Stream(): Stream[Int]).takeWhileViaFoldRight(_ > 0).toList
    )

    printlnBulk(
      "\n",
      "takeWhileViaUnfold:",
      Stream(1, 2, 3, 4).takeWhileViaUnfold(_ > 0).toList,
      Stream(1, 2, -3, 4).takeWhileViaUnfold(_ > 0).toList,
      Stream(-1, 2, -3, 4).takeWhileViaUnfold(_ > 0).toList,
      (Stream(): Stream[Int]).takeWhileViaUnfold(_ > 0).toList
    )

    printlnBulk(
      "\n",
      "map:",
      Stream(1, 2, 3, 4).map(_ * 2).toList
    )

    printlnBulk(
      "\n",
      "filter:",
      Stream(-1, 2, 3, -4).filter(_ > 0).toList,
      Stream(-1, -4).filter(_ > 0).toList
    )

    printlnBulk(
      "\n",
      "flatMap:",
      Stream(1, 2, 3, 4).flatMap(x => Stream(x, x)).toList
    )

    printlnBulk(
      "\n",
      "constant:",
      constant(5).take(5).toList
    )

    printlnBulk(
      "\n",
      "from:",
      from(10).take(10).toList
    )

    printlnBulk(
      "\n",
      "fibs:",
      fibs.take(10).toList
    )

    printlnBulk(
      "\n",
      "fibsViaUnfold:",
      fibsViaUnfold.take(10).toList
    )

    printlnBulk(
      "\n",
      "fromViaUnfold:",
      fromViaUnfold(10).take(10).toList
    )

    printlnBulk(
      "\n",
      "constantViaUnfold:",
      constant(2).take(1000000).toList.length
    )

    //List implementation stack overflows already with numbers > 10000,
    //whereas stream implementation above works well for 1000000
    printlnBulk(
      "\n",
      "constantListViaUnfold:",
      constantLimitedListViaUnfold(2, 1000).length
    )

    printlnBulk(
      "\n",
      "onesViaUnfold:",
      onesViaUnfold.take(10000).toList.length
    )

    printlnBulk(
      "\n",
      "mapViaUnfold:",
      Stream(1, 2, 3, 4).map(_ * 2).toList
    )

    printlnBulk(
      "\n",
      "zipAll:",
      Stream(1, 2, 3, 4).zipAll(Stream(1, 2)).toList,
      Stream(1, 2).zipAll(Stream(1, 2, 3, 4)).toList,
      Stream().zipAll(Stream()).toList
    )
  }

  def printlnBulk[T](ss: T*): Unit =
    for (s <- ss)
      println(s)
}
