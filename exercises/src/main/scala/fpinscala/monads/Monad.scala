package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  //  11.3 Sequence und traverse
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((x, y) => map2(x, y)(_ :: _))


  def sequence_flatMap[A](lma: List[M[A]]): M[List[A]] =
    lma match {
      case Nil => unit(Nil)
      case h :: t => {
        flatMap(h)(hh =>
          map(sequence_flatMap(t))(tt =>
            hh :: tt))
      }
    }

//  def sequence_for[A](lma: List[M[A]]): M[List[A]] =
//    lma match {
//      case Nil => unit(Nil)
//      case h :: t => for {
//        hh <- h
//        tt <- sequence(t)
//      } yield hh :: tt
//    }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la match {
      case Nil => unit(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverseViaSeq[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    sequence_flatMap(la.map(f))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = ???

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = ???

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ???

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  //  11.1 Monad instances für Option, Stream und List
  //  Par und Parser wurden ja garnicht behandelt
  //  val parMonad: Monad[Par] = ???

  //  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  def main(args: Array[String]): Unit = {
    val l = List(Some(2), Some(3))
    val test = optionMonad.sequence_flatMap(l)
    println(test)

    val test2 = optionMonad.sequence(l)
    println(test2)

  }

  //  11.2 Hard: state monad
  def stateMonad[S] = ???

  //  val idMonad: Monad[Id] = ???

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???

  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = ???

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ???
  }
}

