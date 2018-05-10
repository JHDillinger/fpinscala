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

  // product oder factor? wurde sich wohl irgendwann im Buch umentschieden
  //  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
  //    map2(fa, fb)((_, _))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  ////

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))


  //  12.8
  // ok, wie zur Hölle hätte man da draufkommen können?
  // woher weiß ich, dass ich apply overriden muss?
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))

      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) = (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  ////

  //  12.9 not even gonna try
  //  und woher weiß ich hier, dass map2 überschrieben werden muss?
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C) =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

  ////

  //  12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) {
      case (acc, (k, fv)) => map2(acc, fv)(
        (m, v) => m + (k -> v))
    }

  ////
}


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

  //  12.6
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (va, vb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(a, ta), Failure(b, tb)) => Failure(a, ta ++ Vector(b) ++ tb)
      case (_, e@Failure(_, _)) => e
      case (e@Failure(_, _), _) => e
    }

  }

  ////

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
