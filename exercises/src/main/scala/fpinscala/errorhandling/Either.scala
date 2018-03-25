package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
//  4.6
 def map[B](f: A => B): Either[E, B] =
   this match {
     case Left(x) => Left(x)
     case Right(x) => Right(f(x))
   }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
   this match {
     case Left(x) => Left(x)
     case Right(x) => f(x)
   }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
   this match {
     case Left(_) => b
     case Right(x) => Right(x)
   }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   for{
     a <- this
     b1 <- b
   }yield f(a,b1)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
//  4.7
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

//  Warum muss über e nicht gematcht werden? vmtl weil map2 das ja eh schon erledigt. meine Lösung siehe unten:
//      {
//        val e = f(h)
//        e match {
//          case Left(x) => Left(x)
//          case Right(x) => e.map2(traverse(t)(f))(_::_)
//        }
//      }

//  Sequence wieder wie bei Option wenn man traverse schon hat.
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)

  def sequence_lecture[E,A](es: List[Either[E,A]]):Either[E,List[A]] =
    es match {
      case Nil => Right(Nil)
      case h::t => for {
        hh <- h
        tt <- sequence_lecture(t)
      } yield hh::tt
    }

  def traverse_lecture[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    sequence_lecture(es.map(f))

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }


  val l = List("1", "2", "2")
  val parsedS = sequence_lecture(l.map(s => Try(s.toInt)))
  val parsedS2 = sequence(l.map(s => Try(s.toInt)))

  val parsedT = traverse_lecture(l)(s => Try(s.toInt))
  val parsedT2 = traverse(l)(s => Try(s.toInt))


  def main(args: Array[String]): Unit = {
    println(parsedS)
    println(parsedS2)
    println(parsedT)
    println(parsedT2)

  }

}
