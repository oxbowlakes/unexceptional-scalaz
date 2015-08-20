package oxbow

import java.util.concurrent.atomic.AtomicReference

import scalaz.Scalaz._
import scalaz._

package object part9 {
  /*
  If we have to be very careful over using Java methods which may throw Exceptions,
  then we might wish to build our own library
   */
  implicit class AtomicReferenceW[S](self: AtomicReference[S]) {
    type StateTS[M[_], A] = StateT[M, S, A]

    type Transition[M[_], E, A] = EitherT[({type l[a] = StateTS[M, a]})#l, E, A]


  def testAndSet[M[_]: Monad, E, A](p: S => M[Boolean])(s: Transition[M, E, A]): EitherT[M, E, Option[A]] = {
    val M = Monad[M]
    EitherT {
      for {
        s1 <- M.point(self.get)
        s2 <- p(s1).ifM(s.run.run(s1) flatMap {
          case (_, -\/(e))  => \/.left[E, Option[A]](e).point[M]
          case (s2, \/-(a)) => if (!self.compareAndSet(s1, s2)) testAndSet(p)(s).run else \/.right[E, Option[A]](some(a)).point[M]
        }, \/.right[E, Option[A]](none[A]).point[M])
      }                ///////////// inference :-(
        yield s2
    }
  }
}

}
