package fpinscala.streamingio

import fpinscala.iomonad.{IO, Monad, Free, unsafePerformIO}
import language.implicitConversions
import language.higherKinds
import language.postfixOps


object SimpleStreamTransducers {

  /*

We now introduce a type, `Process`, representing pure, single-input
stream transducers. It can be in of three states - it can be
emitting a value to the output (`Emit`), reading a value from its
input (`Await`) or signaling termination via `Halt`.

   */

  sealed trait Process[I,O] {
    import Process._

    /*
     * A `Process[I,O]` can be used to transform a `Stream[I]` to a
     * `Stream[O]`.
     */
    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs) // Stream is empty
      }
      case Emit(h,t) => h #:: t(s)
    }

    /*
     * `Process` can be thought of as a sequence of values of type `O`
     * and many of the operations that would be defined for `List[O]`
     * can be defined for `Process[I,O]`, for instance `map`, `++` and
     * `flatMap`. The definitions are analogous.
     */

    def map[O2](f: O => O2): Process[I,O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => Emit(f(h), t map f)
      case Await(recv) => Await(recv andThen (_ map f))
    }
    def ++(p: => Process[I,O]): Process[I,O] = this match {
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }
    def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
    }

    /*
     * Exercise 5: Implement `|>`. Let the types guide your implementation.
     */
    def |>[O2](p2: Process[O,O2]): Process[I,O2] = ???

    /*
     * Feed `in` to this `Process`. Uses a tail recursive loop as long
     * as `this` is in the `Await` state.
     */
    def feed(in: Seq[I]): Process[I,O] = {
      @annotation.tailrec
      def go(in: Seq[I], cur: Process[I,O]): Process[I,O] =
        cur match {
          case Halt() => Halt()
          case Await(recv) =>
            if (in.nonEmpty) go(in.tail, recv(Some(in.head)))
            else cur
          case Emit(h, t) => Emit(h, t.feed(in))
        }
      go(in, this)
    }


    /*
     * See `Process.lift` for a typical repeating `Process`
     * definition expressed with explicit recursion.
     */

    /*
     * `Process` definitions can often be expressed without explicit
     * recursion, by repeating some simpler `Process` forever.
     */
    def repeat: Process[I,O] = {
      def go(p: Process[I,O]): Process[I,O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }

    def repeatN(n: Int): Process[I,O] = {
      def go(n: Int, p: Process[I,O]): Process[I,O] = p match {
        case Halt() => if (n > 0) go(n-1, this) else Halt()
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(n,recv(i))
        }
        case Emit(h, t) => Emit(h, go(n,t))
      }
      go(n, this)
    }

    /*
     * As an example of `repeat`, see `Process.filter`. We define
     * a convenience function here for composing this `Process`
     * with a `Process` that filters the output type `O`.
     */
    def filter(f: O => Boolean): Process[I,O] =
      this |> Process.filter(f)

    /*
     * Exercise 6: Implement `zipWithIndex`.
     */
    def zipWithIndex: Process[I,(O,Int)] = ???

    /* Add `p` to the fallback branch of this process */
    def orElse(p: Process[I,O]): Process[I,O] = this match {
      case Halt() => p
      case Await(recv) => Await {
        case None => p
        case x => recv(x)
      }
      case _ => this
    }
  }

  object Process {

    case class Emit[I,O](
                          head: O,
                          tail: Process[I,O] = Halt[I,O]())
      extends Process[I,O]

    case class Await[I,O](
                           recv: Option[I] => Process[I,O])
      extends Process[I,O]

    case class Halt[I,O]() extends Process[I,O]

    def emit[I,O](head: O,
                  tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
      Emit(head, tail)

    // Process forms a monad, and we provide monad syntax for it

    import fpinscala.iomonad.Monad

    def monad[I]: Monad[({ type f[x] = Process[I,x]})#f] =
      new Monad[({ type f[x] = Process[I,x]})#f] {
        def unit[O](o: => O): Process[I,O] = emit(o)
        def flatMap[O,O2](p: Process[I,O])(f: O => Process[I,O2]): Process[I,O2] =
          p flatMap f
      }

    // enable monadic syntax for `Process` type
    implicit def toMonadic[I,O](a: Process[I,O]) = monad[I].toMonadic(a)

    /**
      * A helper function to await an element or fall back to another process
      * if there is no input.
      */
    def await[I,O](f: I => Process[I,O],
                   fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
      Await[I,O] {
        case Some(i) => f(i)
        case None => fallback
      }

    /*
     * We can convert any function `f: I => O` to a `Process[I,O]`. We
     * simply `Await`, then `Emit` the value received, transformed by
     * `f`.
     */
    def liftOne[I,O](f: I => O): Process[I,O] =
      Await {
        case Some(i) => emit(f(i))
        case None => Halt()
      }

    def lift[I,O](f: I => O): Process[I,O] =
      liftOne(f).repeat

    /*
     * As an example of `repeat`, here's a definition of `filter` that
     * uses `repeat`.
     */
    def filter[I](f: I => Boolean): Process[I,I] =
      Await[I,I] {
        case Some(i) if f(i) => emit(i)
        case _ => Halt()
      }.repeat

    /*
     * Here's a typical `Process` definition that requires tracking some
     * piece of state (in this case, the running total):
     */
    def sum: Process[Double,Double] = {
      def go(acc: Double): Process[Double,Double] =
        await(d => emit(d+acc, go(d+acc)))
      go(0.0)
    }

    /*
     * Exercise 1: Implement `take`, `drop`, `takeWhile`, and `dropWhile`.
     */
    def take[I](n: Int): Process[I,I] = ???

    def drop[I](n: Int): Process[I,I] = ???

    def takeWhile[I](f: I => Boolean): Process[I,I] = ???

    def dropWhile[I](f: I => Boolean): Process[I,I] = ???

    /* The identity `Process`, just repeatedly echos its input. */
    def id[I]: Process[I,I] = lift(identity)

    /*
     * Exercise 2: Implement `count`.
     */
    def count[I]: Process[I,Int] = ???

    /* For comparison, here is an explicit recursive implementation. */
    def count2[I]: Process[I,Int] = {
      def go(n: Int): Process[I,Int] =
        await((i: I) => emit(n+1, go(n+1)))
      go(0)
    }

    /*
     * Exercise 3: Implement `mean`.
     */
    def mean: Process[Double,Double] = ???

    def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
      await((i: I) => f(i,z) match {
        case (o,s2) => emit(o, loop(s2)(f))
      })

    /* Exercise 4: Implement `sum` and `count` in terms of `loop` */

    def sum2: Process[Double,Double] = ???

    def count3[I]: Process[I,Int] = ???

    /*
     * Exercise 7: Can you think of a generic combinator that would
     * allow for the definition of `mean` in terms of `sum` and
     * `count`?
     */

    def feed[A,B](oa: Option[A])(p: Process[A,B]): Process[A,B] =
      p match {
        case Halt() => p
        case Emit(h,t) => Emit(h, feed(oa)(t))
        case Await(recv) => recv(oa)
      }

    /*
     * Exercise 6: Implement `zipWithIndex`.
     *
     * See definition on `Process` above.
     */

    /*
     * Exercise 8: Implement `exists`
     *
     * We choose to emit all intermediate values, and not halt.
     * See `existsResult` below for a trimmed version.
     */
    def exists[I](f: I => Boolean): Process[I,Boolean] = ???

    /* Awaits then emits a single value, then halts. */
    def echo[I]: Process[I,I] = await(i => emit(i))

    def skip[I,O]: Process[I,O] = await(i => Halt())
    def ignore[I,O]: Process[I,O] = skip.repeat

    def terminated[I]: Process[I,Option[I]] =
      await((i: I) => emit(Some(i), terminated[I]), emit(None))

    def processFile[A,B](f: java.io.File,
                         p: Process[String, A],
                         z: B)(g: (B, A) => B): IO[B] = IO {
      @annotation.tailrec
      def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
        cur match {
          case Halt() => acc
          case Await(recv) =>
            val next = if (ss.hasNext) recv(Some(ss.next))
            else recv(None)
            go(ss, next, acc)
          case Emit(h, t) => go(ss, t, g(acc, h))
        }
      val s = io.Source.fromFile(f)
      try go(s.getLines, p, z)
      finally s.close
    }

    /*
     * Exercise 9: Write a program that reads degrees fahrenheit as `Double` values from a file,
     * converts each temperature to celsius, and writes results to another file.
     */

    def toCelsius(fahrenheit: Double): Double =
      (5.0 / 9.0) * (fahrenheit - 32.0)
  }
}