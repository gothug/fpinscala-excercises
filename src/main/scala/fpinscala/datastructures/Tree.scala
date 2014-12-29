package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // ex 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // ex 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x)      => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // ex 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // ex 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x)      => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // ex 3.29
  def fold[A, B](tree: Tree[A])(fLeaf: A => B)(fBranch: (B, B) => B): B = tree match {
    case Leaf(x)      => fLeaf(x)
    case Branch(l, r) => fBranch(fold(l)(fLeaf)(fBranch), fold(r)(fLeaf)(fBranch))
  }

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(x => 1)((l, r) => 1 + l + r)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)((x: A) => Leaf(f(x)): Tree[B])(Branch(_, _))
}
