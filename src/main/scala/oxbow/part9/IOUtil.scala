package oxbow.part9

import java.nio.file.Path

import scalaz._
import Scalaz._

object IOUtil {

  /*
   This works but is going to return a throwable, you need to Map the throwable yourself
   It's also not explicit about the monad being IO
   */
  def readAllLines1[M[_,_]](p: Path)(implicit M: MonadError[M, Throwable]): M[Throwable, List[String]] = {
    import collection.JavaConverters._
    for {
      pp <- M.point(p)
      ls <- \/.fromTryCatchNonFatal(java.nio.file.Files.readAllLines(pp)).fold(M.raiseError, ls => M.point(ls.asScala.toList))
    } yield ls
  }

  /*
   * We can use MonadIO. Basically this allows us to do IO in a library and yet have the result be an `M` for the call-site `M`
   */
  def readAllLines2[M[_,_]](p: Path)(implicit M: MonadError[M, Throwable], N: effect.MonadIO[({type l[a] = M[Throwable, a]})#l]): M[Throwable, List[String]] = {
    import collection.JavaConverters._

    M.bind(N.liftIO(effect.IO {
      \/.fromTryCatchNonFatal(java.nio.file.Files.readAllLines(p))
    }))(_.fold(M.raiseError, ls => M.point(ls.asScala.toList)))
  }

  /*
   * We actually should use MonadCatchIO. This allows us to call this function from any computational context which has a way
   * of handling errors and performing IO
   */
  def readAllLines3[M[_,_]](p: Path)(implicit M: MonadError[M, Throwable], N: effect.MonadCatchIO[({type l[a] = M[Throwable, a]})#l]): M[Throwable, List[String]] = {
    import collection.JavaConverters._
    N.except(N.liftIO(effect.IO { java.nio.file.Files.readAllLines(p).asScala.toList }))(M.raiseError)
  }

  /**
   * Allow the caller to inject how to translate a Throwable into their own error representation
   */
  def readAllLines[M[_,_], E](p: Path)(mapError: Throwable => E)(implicit M: MonadError[M, E], N: effect.MonadCatchIO[({type l[a] = M[E, a]})#l]): M[E, List[String]] = {
    import collection.JavaConverters._
    N.except(N.liftIO(effect.IO { java.nio.file.Files.readAllLines(p).asScala.toList }))(t => M.raiseError(mapError(t)))
  }

  /* For some reason, scalaz does not supply MonadCatchIO instances for EitherT, ReaderWriterStateT */
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
    val RWSTTMonadIO: MonadIO[({type l[a] = RWST[M, R, W, S, a]})#l] = new MonadIO[({type l[a] = RWST[M, R, W, S, a]})#l] {
      override def point[A](a: => A) = RWST.rwstMonad[M, R, W, S].point(a)

      override def bind[A, B](fa: RWST[M, R, W, S, A])(f: (A) => RWST[M, R, W, S, B]) = RWST.rwstMonad[M, R, W, S].bind(fa)(f)

      override def liftIO[A](ioa: IO[A]) = RWST[M, R, W, S, A]{ (r, s) =>
        val M = MonadIO[M]
        M.liftIO(ioa).map(a => (Monoid[W].zero, a, s))
      }
    }

    override def except[A](ma: RWST[M, R, W, S, A])(handler: (Throwable) => RWST[M, R, W, S, A]) = RWST[M, R, W, S, A]{ (r, s) =>
      val M = MonadCatchIO[M]
      M.bind(M.except(ma.run(r, s) map { \/.right[Throwable, (W, A, S)] })(t => M.point(-\/(t)))) {
        case \/-(x) => M.point(x)
        case -\/(t) => handler(t).run(r, s)
      }
    }

    override def point[A](a: => A) = RWSTTMonadIO.point(a)
    override def bind[A, B](fa: RWST[M, R, W, S, A])(f: (A) => RWST[M, R, W, S, B]) = RWSTTMonadIO.bind(fa)(f)
    override def liftIO[A](ioa: IO[A]) = RWSTTMonadIO.liftIO(ioa)
  }

}
