package fpinscala.testing

import fpinscalabook.state._
import fpinscalabook.state.RNG.Simple

/**
 * Created by kojuhovskiy on 11/02/15.
 */
case class Gen1[A](sample: State[RNG, A])

object Gen1 {
  def choose(start: Int, stopExclusive: Int): Gen1[Int] = {
    val run: RNG => (Int, RNG) = RNG.map(RNG.nonNegativeInt)(_ % 100)
    val state: State[RNG, Int] = State(run)

    Gen1(state)
  }

  def unit[A](a: => A): Gen1[A] = {
    val run: RNG => (A, RNG) = RNG.unit(a)
    val state: State[RNG, A] = State(run)

    Gen1(state)
  }

  def boolean: Gen1[Boolean] = {
    val run: RNG => (Boolean, RNG) = RNG.map(RNG.int)(_ % 2 == 0)
    Gen1(State(run))
  }

  def listOfN[A](n: Int, g: Gen1[A]): Gen1[List[A]] =
    Gen1(State.sequence(List.fill(n)(g.sample)))
}

object Test extends App {
  def genTest[A](gen: Gen1[A]) = {
    def loop(r: RNG, l: List[A]): List[A] = {
      if (l.size > 10) {
        l
      } else {
        gen.sample.run(r) match {
          case (a, rng) =>
            loop(rng, a :: l)
        }
      }
    }

    loop(Simple(1000), List())
  }

  printlnBulk(
    "choose:",
    genTest(Gen1.choose(0, 100)).mkString(" ")
  )

  printlnBulk(
    "unit:",
    genTest(Gen1.unit(66)).mkString(",")
  )

  printlnBulk(
    "boolean:",
    genTest(Gen1.boolean).mkString(",")
  )

  def printlnBulk[T](ss: T*): Unit = {
    println()
    for (s <- ss)
      println(s)
  }
}

