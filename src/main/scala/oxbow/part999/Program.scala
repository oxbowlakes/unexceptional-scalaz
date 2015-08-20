package oxbow.part999

import java.util.logging.Level

import oxbow.support.Logged

import scalaz._; import Scalaz._
abstract class Program[R, S, E] extends Logged {
  type M[A] = ReaderWriterStateT[effect.IO, R, Unit, S, A]
  type Throws[E, A] = EitherT[M, E, A]
  type Returns[A] = Throws[E, A]

  def io[A](f: R => effect.IO[E \/ A]): Returns[A] = EitherT.eitherT[M, E, A](RWST[effect.IO, R, Unit, S, E \/ A]((config, s) => f(config).map(x => ((), x, s))))
  def apply[A](f: R => E \/ A): Returns[A] = io(f andThen (_.point[effect.IO]))

  def either[A](e: E \/ A): Returns[A] = apply(_ => e)
  def eitherIO[A](e: effect.IO[E \/ A]): Returns[A] = io(_ => e)

  def unit[A](a: => A): Returns[A] = either(\/.right(a))
  def fail[A](e: E): Returns[A] = either(\/.left(e))

  def reads[A](f: R => A): Returns[A] = apply(f andThen \/.right)

  def tell(w: => String, level: Level): Returns[Unit] = EitherT.right[M, E, Unit](RWST[effect.IO, R, Unit, S, Unit]((config, s) => effect.IO((logger.log(level, w), (), s))))

  implicit def monadError: MonadError[Throws, E] = EitherT.eitherTMonadError[M, E]
  implicit def monadCatchIO: effect.MonadCatchIO[Returns] = IOUtil.eitherTMonadCatchIO[M, E](IOUtil.rwstMonadCatchIO[effect.IO, R, Unit, S])

  def runUnsafePerformIO[A](program: Returns[A])(r: R, s: S): (E \/ A, S) = {
    (program.run.run(r, s) map { case (_, e, s) => (e, s) }).unsafePerformIO()
  }
  def evalUnsafePerformIO[A](program: Returns[A])(r: R, s: S): E \/ A = {
    (program.run.eval(r, s) map { case (_, e) => e }).unsafePerformIO()
  }
  def evalZeroUnsafePerformIO[A](program: Returns[A])(r: R)(implicit M: Monoid[S]): E \/ A = {
    (program.run.evalZero(r) map { case (_, e) => e }).unsafePerformIO()
  }
  def execUnsafePerformIO[A](program: Returns[A])(r: R, s: S): S = {
    (program.run.exec(r, s) map { case (_, s) => s }).unsafePerformIO()
  }
  def execZeroUnsafePerformIO[A](program: Returns[A])(r: R)(implicit M: Monoid[S]): S = {
    (program.run.execZero(r) map { case (_, s) => s }).unsafePerformIO()
  }
}
