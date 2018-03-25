package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(x) => x
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  //  vs:
  //  map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  //  vs:
  //  this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
  //  if (this.map(f).getOrElse(false)) this
  //  else None
  //  vs:
    this match {
      case Some(a) if f(a) => this
      case _ => None
      //  vs:
      //  flatMap(a => if (f(a)) Some(a) else None)
    }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
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

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //  4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    /*        mean(xs) gibt entweder None zurück oder den mean der Sequence
            Über das ergebnis wird geflatmapt. d.h. falls None -> None
            sonst: gib das m weiter
            berechne den mean einer Seq
            diese Seq entsteht durch mappen der funktion math.pow(x-m,2) über die geg. Liste xs
            der mean dieser Seq ist dann die variance*/
    mean(xs) flatMap (m => {
      mean(xs.map(x => math.pow(x - m, 2)))
    })
  }

  //  4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /*  Das ist die offizielle lösung, aber muss das denn so kompliziert sein?
      ah, ja vermutlich schon, weil ja f auch problematisch sein kann, oder?
      flatMap über a. aa ist der value in A
      warum dann nur map über b?

      flatMap muss sein weil: aa => b.map() returned eine Option[B]
      bb => f() returned einen Wert, deshalb nur map

      ok, jetzt hab ich's: das ergebnis muss in eine Option gewrapped sein
      ABER das manuell zu machen mit Some(f()) ist problematisch
      weil f ja fehlschlagen kann. Würde ich das so machen wie unten, dann könnte
      ein falscher value in Some() stehen
  */

  //  aber warum kann das nicht einfach so aussehen:
  //  (a,b) match {
  //    case (_,None) => None
  //    case (None,_) => None
  //    case (Some(x), Some(y)) => Some(f(x,y))
  //  }

  def map2for[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  //  4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  /*
  Ok, was passiert hier?
  h ist ein Option[A]
  flatmap darüber -> hh ist der value in h
  sequence(t) ist eine Option. über diese option wird gemapped
  map nimmt hh (also den value) und den value in sequence(t) (die LISTE)
  und prepended hh vor LISTE

  Wow, das ist so fucking kompliziert...
  das heißt es wird rekursiv immer ein element aus der List[Option] genommen
  */

  /* vielleicht ist hier die foldRight methode sogar leichter zu verstehen
  * hier wird über die gesamte liste gefolded
  * und im endeffekt mit map2 immer aus zwei elementen eine Option[List(x,y)] gebaut
  * dann wird beim "aufrollen" aus map2(Option[List(w)], Option[List(x,y)]
  * Option[List(w,x,y)]
  *
  */

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  def sequence_lecture[A](as: List[Option[A]]): Option[List[A]] =
  as match {
    case Nil => Some(Nil)
    case h::t => for {
      hh <- h
      tt <- sequence(t)
    } yield (hh :: tt)
  }

  def traverse_lecture[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(l.map(f))

  //  4.5
  /*  Nimm den head, apply f und kombiniere die Option(h) durch map2
      mit der rekursiv aufgebauten liste (map2 wird mit Cons als funktion aufgerufen)
      d.h. map2 wendet Cons auf die INHALTE/VALUES innerhalb der zwei übergebenen
        options an.
      in diesem fall sind das eine Option(value) und eine Option[List(values)]
      cons wird dann mit value und List(values) aufgerufen und konkateniert die
    explicit recursion:*/
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }

  //  foldright:
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  // Sequence via traverse ist einfach nur traverse mit der identitätsfunktion
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def main(args: Array[String]): Unit = {
    val test = Some[Int](4)
    print(test.getOrElse("asd"))

  }
}