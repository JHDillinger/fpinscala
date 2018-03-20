sealed trait Maybe[+A] {
  def map[B](f: A => B): Maybe[B] =
    this match {
      case None => None
      case Just(x) => Just(f(x))
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Just(x) => x
    }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] =
    map(f).getOrElse(None)

  //  vs:
  //  map(f) getOrElse None

  def orElse[B >: A](ob: => Maybe[B]): Maybe[B] =
    this map (Just(_)) getOrElse ob

  //  vs:
  //  this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Maybe[A] =
  //  if (this.map(f).getOrElse(false)) this
  //  else None
  //  vs:
    this match {
      case Just(a) if f(a) => this
      case _ => None
      //  vs:
      //  flatMap(a => if (f(a)) Some(a) else None)
    }
}

case class Just[+A](get: A) extends Maybe[A]
case object None extends Maybe[Nothing]

object Maybe {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Maybe[Double] =
    if (xs.isEmpty) None
    else Just(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Maybe[Double] = {
//    mean(xs) gibt entweder None zurück oder den mean der Sequence
//    Über das ergebnis wird geflatmapt. d.h. falls None -> None
//    sonst: gib das m weiter
//    berechne den mean einer Seq
//    diese Seq entsteht durch mappen der funktion math.pow(x-m,2) über die geg. Liste xs
//    der mean dieser Seq ist dann die variance
    mean(xs) flatMap (m => {
      mean(xs.map(x => math.pow(x-m,2)))
    })
  }


  def map2[A, B, C](a: Maybe[A], b: Maybe[B])(f: (A, B) => C): Maybe[C] =
    (a,b) match {
      case (_,None) => None
      case (None,_) => None
      case (Just(x), Just(y)) => Some(f(x,y))
    }

  def sequence[A](a: List[Maybe[A]]): Maybe[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Maybe[B]): Maybe[List[B]] = ???
}

val test = Seq(1,2,3,4,5,6).map(x => x.toDouble)
Maybe.variance(test)

