package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  //   10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  //  10.2
  //  Problem hier: Die Reihenfolge.
  //  Bei z.B. intAddition oder booleanOr ist das egal, weil die auch kommutativ sind. hier ist die Reihenfolge aber relevant
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  //  deshalb benötigt man hier das duale Objekt des Monoids
  // (hier vllt ein Ausf Diese sind oft lug in die Mathematik?)
  // das Duale erlangt man indem man die Parameter für op() einfach flipt
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  // Now we can have both monoids on hand:
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]

  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  //10.3
  //  Problem hier: Wieder die Reihenfolge. soll es f(g()) (also f compose g) sein oder g(f()) (also f andThen g)?
  //  Da muss man sich einfach entscheiden für eines und wie bei Option kann das andere dann mit "dual" erledigt werden
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f: A => A, g: A => A): A => A = f compose g

    override def zero: A => A = (a: A) => a
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._

  //  10.4 Ok, das fällt natürlich schwer wenn Part 2 nicht behandelt wird in der VL
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  //  10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMapRight[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((a, b) => m.op(f(a), b))

  //  10.6 "Hard"
  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  //  10.7
  def foldMapBalanced[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapBalanced(l, m)(f), foldMapBalanced(r, m)(f))
    }

  //  10.8 nicht wirklich relevant weil Part 2 fehlt (parallel usw)

  //  10.9 deklariert als "hard" vielleicht relevant
  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    ???

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    ???

  //  val wcMonoid: Monoid[WC] = ???

  //  def count(s: String): Int = ???

  //  10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) =
        (A.op(x._1, y._1), B.op(x._2, y._2))

      val zero: (A, B) = (A.zero, B.zero)
    }

  //  10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(f: A => B, g: A => B) = a => B.op(f(a), g(a))

      def zero: A => B = _ => B.zero
    }

  def main(args: Array[String]): Unit = {
    val test = functionMonoid(intAddition)
    val a =test.zero
    val b= test.op(_ =>1,_=> 2)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) {
          (acc, k) =>
            acc.updated(k,
              V.op(
                a.getOrElse(k, V.zero),
                b.getOrElse(k, V.zero)))
        }
    }

  //  10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapBalanced(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
}

// 10.12 komplett alles bis Tree
trait Foldable[F[_]] {

  import Monoid._

  //  endoMonoid[B] geht von B => B
  //  f ist hier deshalb curried weil das f, das in foldMap verwendet wird
  // nur einen Parameter bekommt
  //  d.h. f.curried benutzt dann immer nur den ersten Parameter und gibt B => B zurück --> m.op des endoMonoid
  //  Ich frage mich nur, wo das z hingeht. nimmt f das z an?
  //  ohne das z käme hier B=>B raus, mit z ist es B
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)


  //  ok, das z wird nicht an f übergeben sondern das z ist irgendwie
  //  der Rückgabewert? das gleiche was ich hier mit foldLeft gemacht habe
  // geht auch oben mit foldRight
  //  Aber was genau hat das zur Folge? was ist dieses B das zurückgegeben wird?
  //  wie wird dieses foldLeft dann angewandt und was macht es?
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))
    z
  }

  //  Ok, hier geht foldRight einfach ganz normal wie sonst auch über eine Struktur
  //  foldRight bekommt as, einen "accumulator" (bzw zero)
  // und die Funktion ist einfach die op aus dem übergebenen Monoid
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  //  10.15 genauso wie in kapitel 2 Datastructures
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  import Monoid._

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapBalanced(as, mb)(f)

}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)


  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// 10.13
object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

// 10.14 naja... ist da ein Foldable nötig? was bringt mir das in dem Fall?
// man hat einen "zero" wert und weiß damit, was man bei None ausgeben muss
// aber sonst?
object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as match {
      case None => z
      case Some(a) => f(z, a)
    }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as match {
      case None => z
      case Some(a) => f(a, z)
    }
}

