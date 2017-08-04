package fpinscala
package applicative

import monads.Functor
import monads.Monad
import state._
import State._
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fab, fa)((f, a) => f(a))
  }
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(map(fa)(f.curried))(fb)
  }

  //  def map3[A, B, C, D](fa: F[A],
  //                       fb: F[B],
  //                       fc: F[C])(f: (A, B, C) => D): F[D] =
  //    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  //
  //  def map4[A, B, C, D, E](fa: F[A],
  //                          fb: F[B],
  //                          fc: F[C],
  //                          fd: F[D])(f: (A, B, C, D) => E): F[E]
  //  apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)


  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit())((a, _) => f(a))

  def sequence[A](fas: List[Stream[A]]): Stream[List[A]] = {
    traverse(fas)(fa => fa)
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  }
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa foldLeft unit(Map.empty[K, V]) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }
  }


  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]) = eea match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
    }


  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C) =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: List[E]) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(// Combine elements pointwise
                                                           f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      def unit[A](a: A) = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h2, h1 :: t1 ::: t2)
          case (e @ Failure(_, _), _) => e
          case (_, e @ Failure(_, _)) => e
        }

    }
}


trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>

  import Applicative._

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A): A = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)


  def traverse[M[_] : Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))


  def sequence[M[_] : Applicative, A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def fuse[M[_], N[_], A, B](fa: F[A])(f: A => M[B], g: A => N[B])
                            (implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) = {
    traverse[({type f[x] = (M[x], N[x])})#f, A, B](fa)(a => (f(a), g(a)))(M product N)
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }


}


case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldLeft(M.unit(List[B]()))((fbs, a) => M.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_], A, B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      oa match {
        case Some(a) => M.map(f(a))(Some(_))
        case None => M.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[M[_], A, B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))

  }
}

case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
  def flatMap[B](f: A => OptionT[M,B]): OptionT[M,B] = {
    OptionT(value flatMap {
      case None => M.unit(None)
      case Some(a) => f(a).value
    })
  }
}



