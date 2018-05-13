package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def _map[A, B](s: Rand[A])(f: A => B): RNG => (B, RNG) =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def __map[A, B](s: Rand[A])(f: A => B)(rng: RNG): (B, RNG) = {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  //  6.1
  // meine Lösung. glaub das mit dem rekursiven neuen aufruf
  //  ist nicht gerade das, was man haben will wenn man funktional programmiert. bzgl referential transparency und so?
  /*  def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i1, rng2) = rng.nextInt
      i1 match {
        case Int.MinValue => nonNegativeInt(rng2)
        case x if x < 0 => (Math.abs(i1), rng2)
        case _ => (i1,rng2)
      }
    }*/
  //  buchlösung
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  //  6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / Int.MaxValue.toDouble + 1, r)
  }

  // 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  //  6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @annotation.tailrec
    def loop(n: Int, xs: List[Int], r: RNG): (List[Int], RNG) = {
      if (n <= 0)
        (xs, r)
      else {
        val (x, r) = rng.nextInt
        loop(n - 1, x :: xs, r)
      }
    }

    loop(count, List(), rng)
  }

  //  6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }


  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): RNG => (C, RNG) =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def __map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C)(rng: RNG): (C, RNG) = {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  //  6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))


  //  6.8 (flatMap + nonNegativeLessThan mit flatMap)
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1) // We pass the new state along
    //  6.8
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  //  6.9 map and map2 via flatMap
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def main(args: Array[String]): Unit = {
    val rng = Simple(42)
    println(nonNegativeInt(rng))
    //    val test = double(rng)
    //    println(test)

    val a = map(int)(_ * 2)(rng)
    val b = _map(int)(_ * 2)(rng)
    val c = __map(int)(_ * 2)(rng)
    println(a)
    println(b)
    println(c)
    println(a == b && b == c)
  }
}
