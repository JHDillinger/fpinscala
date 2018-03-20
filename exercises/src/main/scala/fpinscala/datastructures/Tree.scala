package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //  3.25
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  //  3.26
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  //  3.27
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  //  3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  //  3.29
  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B =
    t match {
      case Leaf(x) => l(x)
      case Branch(x, y) => b(fold(x)(l)(b), fold(y)(l)(b))
    }

  def foldedSize[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => 1 + l + r)

  def foldedMax(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def foldedDepth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => 1 + (l max r))

  def foldedMap[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))

/*  val t = Branch(
    Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))
  )

  Tree.foldedSize(t)
  Tree.foldedMax(t)
  Tree.foldedDepth(t)
  Tree.foldedMap(t)(x => x + 1)*/

}

