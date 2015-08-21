package oxbow.part999

import java.nio.file.Path

import scalaz.Scalaz._
import scalaz._

object IOUtil {

   def readAllLines[M[_,_], E](p: Path)(mapError: Throwable => E)(implicit M: MonadError[M, E], N: effect.MonadCatchIO[({type l[a] = M[E, a]})#l]): M[E, List[String]] = {
     import collection.JavaConverters._
     N.except(N.liftIO(effect.IO { java.nio.file.Files.readAllLines(p).asScala.toList }))(t => M.raiseError(mapError(t)))
   }

   import effect._
   implicit def eitherTMonadCatchIO[M[_]: MonadCatchIO, E] = new MonadCatchIO[({type l[a]=EitherT[M, E, a]})#l] {
     val EitherTMonadIO = MonadIO.eitherTMonadIO[M, E]
     override def except[A](ma: EitherT[M, E, A])(handler: Throwable => EitherT[M, E, A]) = EitherT[M, E, A] {
       val M = MonadCatchIO[M]
       val a: M[Throwable \/ (E \/ A)] = M.except(ma.run.map(\/.right[Throwable, E \/ A]))( t => M.point(-\/(t)))
       val b: M[E \/ A] = M.bind(a) {
         case -\/(t) => handler(t).run
         case \/-(r) => M.point(r)
       }
       b
     }

     override def point[A](a: => A) = EitherTMonadIO.point(a)
     override def bind[A, B](fa: EitherT[M, E, A])(f: (A) => EitherT[M, E, B]) = EitherTMonadIO.bind(fa)(f)
     override def liftIO[A](ioa: IO[A]) = EitherTMonadIO.liftIO(ioa)
   }

  implicit def rwstMonadCatchIO[M[_]: MonadCatchIO, R, W: Monoid, S] = new MonadCatchIO[({type l[a] = RWST[M, R, W, S, a]})#l] {
    override def except[A](ma: RWST[M, R, W, S, A])(handler: (Throwable) => RWST[M, R, W, S, A]) = RWST[M, R, W, S, A]{ (r, s) =>
      val M = MonadCatchIO[M]
      M.bind(M.except(ma.run(r, s) map { \/.right[Throwable, (W, A, S)] })(t => M.point(-\/(t)))) {
        case \/-(x) => M.point(x)
        case -\/(t) => handler(t).run(r, s)
      }
    }

    override def point[A](a: => A) = RWST.rwstMonad[M, R, W, S].point(a)
    override def bind[A, B](fa: RWST[M, R, W, S, A])(f: (A) => RWST[M, R, W, S, B]) = RWST.rwstMonad[M, R, W, S].bind(fa)(f)
    override def liftIO[A](ioa: IO[A]) = RWST[M, R, W, S, A]{ (r, s) =>
      val M = MonadIO[M]
      M.liftIO(ioa).map(a => (Monoid[W].zero, a, s))
    }
  }

 }
