package fpinscala.state

// 6.10
case class State[S, +A](run: S => (A, S)) {

  import State._

  //  6.10 generalize map, map2 and flatMap
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {
  //  6.10 generalize unit and sequence
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }

    State((s: S) => go(s, sas, List()))
  }

  type Rand[A] = State[RNG, A]


  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  val getString: State[String, String] = State(s => (s, s))
  // String > (String, String)

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  val intProg: State[Int, Int] = for {
    //    _ <- get
    _ <- modify[Int](_ + 1)
    _ <- modify[Int](_ * 3)
    _ <- modify[Int](_ - 2)
    result <- get
  } yield result

  val intProg2: State[Int, Int] = for {
    _ <- set[Int](3)
    _ <- modify[Int](_ + 1)
    _ <- modify[Int](_ * 3)
    _ <- modify[Int](_ - 2)
    result <- get
  } yield result

  val stringProg: State[String, String] =
    for {
      _ <- modify[String](_ ++ " World")
      _ <- modify[String](_ ++ " Program!")
      result <- get
    } yield result

  val stringProg4: State[String, String] =
    modify[String](_ ++ "world").flatMap(
      _ => modify[String](_ ++ "Program!")).flatMap(_ => get)

  def stringProg3(start: String) = State[String, String](_ => (start, start)).flatMap(
    _ => modify[String](_ ++ " World").flatMap(
      _ => modify[String](_ ++ " Program!")).map(_ => get)).run(start)

  def stringProg2(str: List[String]): State[String, String] = for {
    _ <- sequence(str.map(s => modify[String](_ ++ s)))
    result <- get
  } yield result


  def main(args: Array[String]): Unit = {
    val s = stringProg.run("Hello")._1
    println(s)
    val strings = List(" ", "World", " ", "Program", "!")
    val test = stringProg2(strings).run("Hello")._1
    println(test)

    val st = State[Int, Int](x => (2, x))
    val g = st.run(5)
    println(g)

    println(stringProg3("test"))
    println(stringProg4.run("test"))
    println(stringProg.run("test"))


    val getter = get[String] // State(s => (s, s))


    // Sinnfreies Programm, das den eingehenden String (state) zu Großbuchstaben modifiziert
    // Anschließend mit get den aktuellen State nimmt
    // Und mit set den State den String dupliziert
    val runC = for {
      _ <- modify[String](_.toUpperCase)
      s <- get
      r <- set(s ++ s)
    } yield r
    println(runC.run("bla")) // ((), BLABLA)

    val runFM =
      modify[String](_.toUpperCase)
        .flatMap(_ => get
          .flatMap(s => set(s ++ s)))
    println(runFM.run("bla")) // ((), BLABLA)


    val run = (state0: String) => {
      val (wert1, state1) = ((s: String) => ((), s.toUpperCase)) (state0) // modify
      val (wert2, state2) = ((s: String) => (s, s)) (state1) // get
      val (wert3, state3) = ((s: String) => ((), wert2 ++ wert2)) (state2) // set
      (wert3, state3)
    }
    println(run("bla")) // ((), BLABLA)


  }
}


