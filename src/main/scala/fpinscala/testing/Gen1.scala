package fpinscala.testing

import fpinscala.state._
import fpinscala.state.RNG.Simple

/**
 * Created by kojuhovskiy on 11/02/15.
 */
case class GenA[A](sample: State[RNG, A])

object GenA {
  def choose(start: Int, stopExclusive: Int): GenA[Int] = {
    val run: RNG => (Int, RNG) = RNG.map(RNG.nonNegativeInt)(_ % 100)
    val state: State[RNG, Int] = State(run)

    GenA(state)
  }

  def unit[A](a: => A): GenA[A] = {
    val run: RNG => (A, RNG) = RNG.unit(a)
    val state: State[RNG, A] = State(run)

    GenA(state)
  }
}

object Test extends App {
  val gen = GenA.choose(0, 100)

  var s: RNG = Simple(1000)
  var n = -1

  printlnBulk(
    "choose:"
  )

  for (i <- 1 to 10) {
    val r: (Int, RNG) = gen.sample.run(s)

    n = r._1
    s = r._2

    println(n)
  }

  def printlnBulk[T](ss: T*): Unit = {
    println()
    for (s <- ss)
      println(s)
  }
}

