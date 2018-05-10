package fpinscala
package applicative


import monads.Functor
import state._
//import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions


trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence[G, B](map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  //  12.14
  type ID[A] = A

  val idMonad = new Monad[ID] {
    def unit[A](a: => A) = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[ID, A, B](fa)(f)(idMonad)

  ////

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  //  12.16
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  ////

  //  12.17
  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  ////

  //  12.18
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G.product(H))

  ////

  //  12.19
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
    val self = this

    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }
  }
}

////

case class Tree[+A](head: A, tail: List[Tree[A]])


object Traverse {
  //  12.13
  val listTraverse = new Traverse[List] {
    //    auto completed override:
    //    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = super.traverse(fa)(f)

    //    vs:
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None => G.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }

  ////
}

//object StateUtil {
//
//  def get[S]: State[S, S] =
//    State(s => (s, s))
//
//  def set[S](s: S): State[S, Unit] =
//    State(_ => ((), s))
//}