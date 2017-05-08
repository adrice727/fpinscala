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

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG, A])

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(x => start + x % (stopExclusive - start)))
  }

  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listofN[A](n: Int, g: Gen[A]): List[Gen[A]] = ???
}

