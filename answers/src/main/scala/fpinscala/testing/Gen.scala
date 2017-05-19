package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}
import language.postfixOps
import language.implicitConversions

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => {
      run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case f => f
      }
    }
  }
  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => {
      run(max, n, rng) match {
        case Passed => Passed
        case _ => p.run(max, n, rng)
      }
    }
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests")
    }

  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack tract: \n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
  def map[B](f: A => B): SGen[B] = SGen {
    g(_) map f
  }
  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap {
        f(_).g(n)
      }
    }
    SGen(g2)
  }
  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n) ** s2(n))
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(sample.map2(g.sample)(f))
  }


  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))


  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(x => start + x % (stopExclusive - start)))
  }

  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double)).flatMap(x => if (x < threshold) g1._1 else g2._1)
  }

}


