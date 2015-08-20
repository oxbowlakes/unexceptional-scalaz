package oxbow.part999

import java.nio.file.Path

import scalaz._; import Scalaz._

class NoHeaderException extends Exception

object Csv {

  type Header = String; type Cell = String; type Index = Int

  def from[M[_,_], E](lines: Stream[String])(mapError: Throwable => E)(implicit M: MonadError[M, E]): M[E, Csv] = lines match {
    case Stream.Empty     => M.raiseError(mapError(new NoHeaderException))
    case header #:: rows  => M.point(Csv(header.split(",").toStream.map(_.trim).zipWithIndex.toMap, rows))
  }

  def parseRunZero[M[_,_], E, S, A](lines: Stream[String])(mapError: Throwable => E)(line: Map[Header, Cell] => State[S, A])(implicit M: MonadError[M, E], S: Monoid[S]): M[E, (S, List[A])]
    = M.bind(from[M, E](lines)(mapError))(csv => csv.parseZero[M, E, S, A](line)(mapError))
  def parseEvalZero[M[_,_], E, S, A](lines: Stream[String])(mapError: Throwable => E)(line: Map[Header, Cell] => State[S, A])(implicit M: MonadError[M, E], S: Monoid[S]): M[E, List[A]]
    = M.map(parseRunZero(lines)(mapError)(line))(_._2)
  def parseExecZero[M[_,_], E, S, A](lines: Stream[String])(mapError: Throwable => E)(line: Map[Header, Cell] => State[S, A])(implicit M: MonadError[M, E], S: Monoid[S]): M[E, S]
    = M.map(parseRunZero(lines)(mapError)(line))(_._1)

  def parseExecZeroIO[M[_,_], E, S, A](path: Path)(mapError: Throwable => E)(line: Map[Header, Cell] => State[S, A])(implicit M: MonadError[M, E], S: Monoid[S], C: effect.MonadCatchIO[({type l[a] = M[E, a]})#l]): M[E, S]
    = M.bind(IOUtil.readAllLines[M, E](path)(mapError)){ lines => parseExecZero[M, E, S, A](lines.toStream)(mapError)(line)}
}

import Csv._
case class Csv(indices: Map[Header, Csv.Index], rows: Stream[String]) {
  def parse[M[_, _], E, S, A](line: Map[Header, Cell] => State[S, A])(s: S)(mapError: Throwable => E)(implicit M: MonadError[M, E]): M[E, (S, List[A])] = {

    val a: Throwable \/ (S, List[A]) =
      \/.fromTryCatchNonFatal(rows.toList.traverseU({ l =>
        val cells = l.split(",")
        line(indices.mapValues(cells))
      }).run(s))

    a.fold(mapError andThen M.raiseError, x => M.point(x))
  }

  def parseZero[M[_, _], E, S, A](line: Map[Header, Cell] => State[S, A])(mapError: Throwable => E)(implicit M: MonadError[M, E], S: Monoid[S]): M[E, (S, List[A])]
    = parse(line)(S.zero)(mapError)
}

