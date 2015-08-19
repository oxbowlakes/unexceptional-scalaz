package oxbow.part8

import java.util.concurrent.atomic.AtomicReference

import scalaz._; import Scalaz._
package object outro {
  implicit class AtomicReferenceW[S](val self: AtomicReference[S]) extends AnyVal {
    type StateTS[M[_], A] = StateT[M, S, A]

    type Transition[M[_], E, A] = EitherT[({type l[a] = StateTS[M, a]})#l, E, A]


    def testAndSet1[M[_]: Monad, E, A](p: S => M[Boolean])(s: Transition[M, E, A]): EitherT[M, E, Option[A]] = {
      val M = Monad[M]
      EitherT {
        for {
          s1 <- M.point(self.get)
          b <- p(s1)                                              /// Do you update state even if a fail? Probably Not
          s2 <- if (b) s.run.run(s1) flatMap { case (s2, eOrA) => if (!self.compareAndSet(s1, s2)) testAndSet1(p)(s).run else eOrA.map(some).point[M] } else \/.right(none[A]).point[M]
        }
          yield s2
      }
    }
    def testAndSet[M[_]: Monad, E, A](p: S => M[Boolean])(s: Transition[M, E, A]): EitherT[M, E, Option[A]] = {
      val M = Monad[M]
      EitherT {
        for {
          s1 <- M.point(self.get)
          b <- p(s1)
          s2 <- if (b) s.run.run(s1) flatMap {
            case (_, -\/(e))  => \/.left[E, Option[A]](e).point[M]
            case (s2, \/-(a)) => if (!self.compareAndSet(s1, s2)) testAndSet(p)(s).run else \/.right[E, Option[A]](some(a)).point[M]
          } else \/.right(none[A]).point[M]
        }
          yield s2
      }
    }
  }
}
