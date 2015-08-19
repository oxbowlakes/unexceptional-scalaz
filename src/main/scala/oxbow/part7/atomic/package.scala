package oxbow.part7

package object atomic {
  import java.util.concurrent.atomic.AtomicReference

  import scalaz._; import Scalaz._

  implicit class AtomicReferenceW[S](val self: AtomicReference[S]) extends AnyVal {

    //Want to do some form of test-and-set

    def testAndSet(p: S => Boolean)(newV: => S): Unit = {
      val s = self.get
      if (p(s)) {
        if (!self.compareAndSet(s, newV)) testAndSet(p)(newV)
      }
    }

    // What if we were setting the new value from a State transition and returning a result?

    def testAndSetS[A](p: S => Boolean)(s: State[S, A]): Option[A] = {
      val s1 = self.get
      if (p(s1)) {
        val (s2, a) = s.run(s1)
        if (!self.compareAndSet(s1, s2)) testAndSetS(p)(s) else Some(a)
      }
      else
        None
    }

    def testAndSetM1[M[_]: Monad, A](p: S => Boolean)(s: StateT[M, S, A]): OptionT[M, A] = {
      val s1 = self.get
      if (p(s1)) {
        OptionT {
          s.run(s1) flatMap { case (s2, a) =>
            if (!self.compareAndSet(s1, s2)) testAndSetM1(p)(s).run else some(a).point[M]
          }
        }
      }
      else OptionT.none[M, A]
    }

    //Actually not happy with this; it does stuff like access/test reference outside of M

    def testAndSetM[M[_]: Monad, A](p: S => M[Boolean])(s: StateT[M, S, A]): OptionT[M, A] = {
      val M = Monad[M]
      OptionT {
        for {
          s1 <- M.point(self.get)
          b <- p(s1)
          s2 <- if (b) s.run(s1) flatMap { case (s2, a) => if (!self.compareAndSet(s1, s2)) testAndSetM(p)(s).run else some(a).point[M] } else none[A].point[M]
        }
          yield s2
      }
    }
  }

}
