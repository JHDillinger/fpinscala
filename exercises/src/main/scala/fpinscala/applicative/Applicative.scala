package fpinscala
package applicative

import monads.Functor
import state._
//import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]


  //  12.2 ////

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
  //    map2(fab, fa)(_ (_)) äquivalent zu:
    map2(fab, fa)((f, x) => f(x))

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  ////

  //  12.3 ////
  //Irgendwie seltsame Aufgabe. kann das innerste apply(unit( )) ersetzt werden durch map?
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)


  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])
                         (f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)


  ////


  // Map aus Listing 12.1
  def _map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  // 12.1 ////
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  ////

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = ???

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = ???

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ???
}

case class Tree[+A](head: A, tail: List[Tree[A]])


sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                            f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = ???

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}


// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
