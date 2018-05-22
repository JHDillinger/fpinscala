package fpinscala.monoids

import language.higherKinds


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
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

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

  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapBalanced(as, mb)(f)

}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)


  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

// 10.12 komplett alles bis Tree


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