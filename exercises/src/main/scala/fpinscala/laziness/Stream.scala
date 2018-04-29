package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //  5.1 Variante a) führt aber zu stack overflow weil nicht tail-recursive
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _ => List()
  }

  //  die variante geht, aber es muss halt am ende reversed werden
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  //  5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

//  Ohne pattern guard
  def take2(n: Int): Stream[A] = this match {
    case Cons(h, t) => {
      if (n > 0) cons(h(), t().take2(n - 1))
      else if (n == 1) cons(h(), empty)
      else empty
    }
    //    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  //  um das ganze tailrecursive machen zu können muss ein final davor
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) t().drop(n - 1) else this
    case _ => this
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
    case _ => empty
  }

  //  5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  //  5.5
  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => {
      if (p(h)) cons(h, t)
      else empty
    })

  //  5.6 (hard)
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  //  [B>:A] sagt: B ist ein supertype von A
  //  Das muss sein, weil Stream[+A]
  //  das +A macht das ganze covariant
  //  Hier: Elemente vom Typ B dürfen an einen Stream von Typ A
  //  appended werden, aber das Resultat ist dann ein Stream[B]
  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  //  5.13
  //  Lohnt sich die scheiße hier überhaupt?
  //  das ist doch alles soooo verbose...
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  //  zip wenn zwei streams zu einem stream an tuples werden soll
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))


  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  //  Diese "->" Pfeile sind wohl nur syntactic sugar für Tuples?
  //  also (a -> b) = (a,b)
  //   nicht sicher, ob die das ganze lesbarer machen...
  //  ist eh schon furchtbar
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) =>
        Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) =>
        Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  //  5.14 Hard
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  //  5.15
  //  geht das nicht auch anders?
  //  mit pattern matching auf Cons(h,t) => Some(Cons(h,t), t())
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  //  5.16

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  @annotation.tailrec
  def drop[A](s:Stream[A])(n: Int): Stream[A] = s match {
    case Cons(h, t) => if (n > 0) drop(t())(n - 1) else s
    case _ => s
  }
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  //  5.8
  def constant[A](a: A): Stream[A] = {
    //    Variante a)
    //    cons(a, constant(a))
    //    effizienter:
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // 5.9
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  //  5.10
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }
  }

  //  5.12
  //  eigentlich müsste man p => p match{ case } schreiben
  //  das kann aber abgekürzt werden, "p" braucht man ja sonst nirgends
  val fibsViaUnfold =
  unfold((0, 1)) {
    case (f0, f1) => Some((f0, (f1, f0 + f1)))
  }

  //  warum ist die verwendung von "n" im zweiten argument von unfold
  //  kein "suspicious shadowing"?
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  val onesViaUnfold = constantViaUnfold(1)
  //  oder:
  //  val onesViaUnfold = unfold(1)(_ => Some((1,1)))

  def main(args: Array[String]): Unit = {
    val test = Stream(1, 2, 3, 3, 4)

    println(test.tails.toList)
    test.tails

  }
}
