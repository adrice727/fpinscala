package fpinscala.monoids

import scala.annotation.tailrec

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  import fpinscala.testing._
  import fpinscala.testing.Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      forAll(gen)((a: A) =>
        m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  val listMonoid = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    def zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    def op(f: A => A, g: A => A) = f compose g
    val zero = (a: A) => a
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))
  }

  def foldRightViafoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty)
      m.zero
    else if (as.length == 1)
      f(as(0))
    else {
      val (left, right) = as.splitAt(as.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  import fpinscala.parallelism.Nonblocking._
  import fpinscala.parallelism.Nonblocking.Par._

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero = Par.unit(m.zero)
    def op(a: Par[A], b: Par[A]): Par[A] = a.map2(b)(m.op)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.async(b))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {

    val a = Some((0, 0))
    val o = optionMonoid
    // Ordered Option => (min, max, ordered)
    val orderedMon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) = (o1, o2) match {
        case(Some((x1, y1, b1)), Some((x2, y2, b2))) => Some((x1 min x2, y1 max y2, b1 && b2 && y1 <= x2))
        case (None, x) => x
        case (x, None) => x
      }
      def zero = None
    }
    foldMapV(ints, orderedMon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a: WC, b: WC): WC = (a, b) match {
      case(Stub(x), Stub(y)) => Stub(x + y)
      case(Part(l,w,r), Stub(y)) => Part(l, w, r + y)
      case(Stub(x), Part(l, w, r)) => Part(x + l, w, r)
      case(Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
    def zero = Stub("")
  }

  def count(input: String): Int = {
    def wc(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    def unStub(s: String) = s.length min 1

    foldMapV(input.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(x) => unStub(x)
      case Part(l, w, r)  => unStub(l) + w + unStub(r)
    }
  }
}

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
}

object ListFoldable extends Foldable[List] {
  def foldRight[A,B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A,B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A,B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b,a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A,B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A,B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  def foldRight[A,B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A,B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A,B](as: Stream[A])(f: A => B)(mb: Monoid[B]) = as.foldLeft(mb.zero)((b,a) => mb.op(b, f(a)))
}

