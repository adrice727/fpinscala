package fpinscala.iomonad

import fpinscala.monads._

object IO {

  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B])
      extends TailRec[B]

  object TailRec extends Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)
    def flatMap[A, B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
      a flatMap f
    def suspend[A](a: => TailRec[A]) =
      Suspend(() => ()).flatMap { _ =>
        a
      }

  }

  @annotation.tailrec
  def run[A](t: TailRec[A]): A = t match {
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match {
        case Return(a)     => run(f(a))
        case Suspend(r)    => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
  }
}

object IO2 {

  import fpinscala.parallelism.Nonblocking._

  sealed trait Async[A] { // will rename this type to `Async`
    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)
    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A] // notice this is a `Par`
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  object Async extends Monad[Async] {
    def unit[A](a: => A): Async[A] = Return(a)
    def flatMap[A, B](a: Async[A])(f: A => Async[B]): Async[B] = a flatMap f
  }

  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a)  => Par.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) =>
      x match {
        case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
        case _          => sys.error("Impossible, since `step` eliminates these cases")
      }
  }

}

object IO3 {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B])
      extends Free[F, B]

  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] =
    new Monad[({ type f[a] = Free[F, a] })#f] {
      def unit[A](a: => A) = Return(A)
      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
        fa flatMap f
    }
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match {
        case Return(a)  => runTrampoline { f(a) }
        case Suspend(r) => runTrampoline { f(r()) }
        case FlatMap(b, g) =>
          runTrampoline {
            b flatMap { b =>
              g(b) flatMap f
            }
          }
      }
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a)              => F.unit(a)
    case Suspend(r)             => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
  }

  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => a
  }

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)
    def toThunk: () => run
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]
    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Option[String]] =
      Suspend(PrintLn(line))
  }

  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]) = a.toThunk
  }
  val consoleToPar = new (Console ~> Par) {
    def apply[A](a: Console[A]) = a.toPar
  }

  def runFree[F[_], G[_]](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]) = {
    step(free) match {
      case Return(a)              => G.unit(a)
      case Suspend(r)             => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ =>
        sys.error("We should never be here because of the step implementation")
    }
  }

  def runConsoleFuntion0[A](a: Free[Console, A]): () => A = {
    runFree[Console, Function0, A](a)(consoleToFunction0)
  }

  def runConsolePar[A](a: Free[Console, A]): () => A = {
    runFree[Console, Par, A](a)(consoleToPar)
  }

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]) = { () =>
      f(a())()
    }
  }

  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A, B](a: Par[A])(f: A => Par[B]) = {
      Par.fork { Par.flatMap(a)(f) }
    }
  }

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G,A] = {
    type FreeG[A] = Free[G,A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G,A] = Suspend { fg(a) }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A = {
    runTrampoline { translate(a)( new (Console ~> Function0) {
      def apply[A](c: Console[A]) = c.toThunk
    })}
  }
}
