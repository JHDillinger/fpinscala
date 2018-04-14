package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  /*  def sum2(ns: List[Int]):List[Int] =
      foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]):List[Double] =
      foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar*/

  //3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }

  //  3.3
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("empty list")
      case Cons(_, xs) => Cons(h, xs)
    }

  //  3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)

      }

  //  3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }

  //  3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("empty list")
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  //  3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => 1 + acc)

  //3.10
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // 3.11
  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, h) => acc + 1)

  //3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((x, y) => Cons(y, x))

  //  3.14
  //   Das war nicht gefragt, man sollte eine ganze liste appenden
  def appendItem[A](l: List[A], item: A): List[A] =
  //    foldRight(l, Cons(item,Nil))((x,y) => Cons(x,y))
    foldRight(l, Cons(item, Nil))(Cons(_, _))

  //  für liste:
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  //  3.15 concatenates a list of list into single list
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  //  3.16
  def incrementAll(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  //  3.17
  def stringifyAll(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  //  3.18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  //  implementierung mit local mutation
  def map_2[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }

    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  //  3.19
  //  gleiches thema wie bei map. foldRight (in dieser implementierung) ist nicht stack safe.
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => {
      if (f(h)) Cons(h, t)
      else t
    })

  //  3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  //  mein approach war zuerst, das direkt mit foldRight zu implementieren
  def flatMapViaFR[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((h, t) => append(f(h), t))

  // 3.21
  //  def flatMapFilter

  // 3.22
  def zipAdd(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, zipAdd(ta, tb))
    }

  //  3.23
  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }


  val l = List[Int](1, 2, 3, 4)
  assert(List.tail(l) == List(2, 3, 4))
  assert(List.drop(l, 2) == List(3, 4))
  assert(List.dropWhile(l, (x: Int) => x < 3) == List(3, 4))
  assert(List.init(l) == List(1, 2, 3))

}

object Main {
  def main(args: Array[String]): Unit = {
    val test = List.flatMapViaFR(List(1, 2, 3))(i => List(i, i * 10)) == List(1, 10, 2, 20, 3, 30)

    val test2 = List.foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, y) => x + y)
    val test3 = List.foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, y) => x + y)


    println(test2)
    println(test3)
  }
}