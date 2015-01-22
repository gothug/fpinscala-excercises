package fpinscala.state

import RNG._

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => { rng => (f(a), rng) })

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    
    (i / (Int.MaxValue.toDouble + 1), r)
  }
  
  def _double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)

    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, r: RNG, l: List[Int]): (List[Int], RNG) =
      if (n <= 0) (l, rng)
      else {
        val (i, rr) = r.nextInt
        loop(n - 1, rr, i :: l)
      }
    
    loop(count, rng, Nil)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, raa) = ra(rng)
      val (b, rbb) = rb(raa)
      
      (f(a, b), rbb)
    }
  
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      def loop(as: List[Rand[A]], r: RNG, lst: List[A]): (List[A], RNG) =
        as match {
          case h :: t =>
            val (v, rr) = h(r)
            loop(t, rr, lst ::: List(v))
          case _ => (lst, r)
        }
      
      loop(fs, rng, Nil)
    }
  
  def sequenceViaFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((a: Rand[A], r: Rand[List[A]]) => map2(a, r)((a: A, b: List[A]) => a :: b))
//    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      
      g(a)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
object Test {
  def main(args: Array[String]): Unit = {
    printlnBulk(
      "nonNegativeInt:",
      nonNegativeInt(Simple(42))
    )

    printlnBulk(
      "double:",
      double(Simple(2000))
    )

    printlnBulk(
      "intDouble:",
      intDouble(Simple(2000))
    )

    printlnBulk(
      "doubleInt:",
      doubleInt(Simple(2000))
    )

    printlnBulk(
      "double3:",
      double3(Simple(2000))
    )

    printlnBulk(
      "ints:",
      ints(10)(Simple(2000))
    )

    printlnBulk(
      "sequence:",
      sequence(List(unit(1), unit(2), unit(3)))(Simple(100))._1 == List(1, 2, 3)
    )

    printlnBulk(
      "sequenceViaFoldRight:",
      sequenceViaFoldRight(List(unit(1), unit(2), unit(3)))(Simple(100))._1 == List(1, 2, 3)
    )

    printlnBulk(
      "mapViaFlatMap:",
      mapViaFlatMap(_double)(_ * -1) { Simple(200) }
    )
  }

  def printlnBulk[T](ss: T*): Unit = {
    println()
    for (s <- ss)
      println(s)
  }
}
