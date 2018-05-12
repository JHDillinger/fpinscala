package fpinscala.monoids


sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    // The empty result, where we haven't seen any characters yet.
    val zero = Stub("")

    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unstub(s: String) = s.length.min(1)

    val m = Monoid.foldMapBalanced(s.toIndexedSeq, wcMonoid)(wc)

    m match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def main(args: Array[String]): Unit = {
    val t = count("")
    println(t)

    val s = "das ist ein satz".toIndexedSeq

  }

}