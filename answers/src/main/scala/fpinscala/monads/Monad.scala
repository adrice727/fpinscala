package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
  def map[A, B](fa: F[A])(f: (A) => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldLeft(unit(List[A]()))((fla, fa) => map2(fa, fla)(_ :: _))
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldLeft(unit(List[B]()))((fb, a) => map2(f(a),fb)(_ :: _))
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))
  def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] =
    map2(ma, mb)((a,b) => (a,b))
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil => unit(Nil)
    case h :: t => flatMap(f(h))(b =>
      if (!b) filterM(t)(f)
      else map(filterM(t)(f))(h :: _))
  }
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    compose((_: Unit) => ma, f)(())
  }
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Monad {
  val ParMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  val GenMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    override def flatMap[A,B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

  // Since `State` is a binary type constructor, we need to partially apply it
  // with the `S` type argument. Thus, it is not just one monad, but an entire
  // family of monads, one for each type `S`. One solution is to create a class
  // `StateMonads` that accepts the `S` type argument and then has a _type member_
  // for the fully applied `State[S, A]` type inside:
  class StateMonad[S] {
    type StateS[A] = State[S, A]

    // We can then declare the monad for the `StateS` type constructor:
    val monad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
  }

  // But we don't have to create a full class like `StateMonads`. We can create
  // an anonymous class inline, inside parentheses, and project out its type member,
  // `lambda`:
  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a,s))
    def flatMap[A,B](st: State[S, A])(f: A => State[S,B]): State[S,B] = st flatMap f
  }
}

case class Reader[R,A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = Reader(r => f(st.run(r)).run(r))
  }
}
