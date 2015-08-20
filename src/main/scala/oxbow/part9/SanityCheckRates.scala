package oxbow.part9

import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Executors, TimeUnit}

import oxbow.support.{Currency, Logged, UnderlyingThrowable}

import scalaz.Scalaz._
import scalaz._
import scalaz.effect.IO

object SanityCheckRates extends App with Logged {
    sealed trait E
    case class NoSuchResource(path: String) extends E
    case class IOException(underlying: Throwable) extends E with UnderlyingThrowable
    def ioException(t: Throwable): E = IOException(t)
    case object NoHeader extends E
    case class ParseException(t: Throwable) extends E
    case class MismatchedRates(ccy: Currency, r1: BigDecimal, r2: BigDecimal) extends E

    class Ticker(val symbol: String) extends AnyVal
    case class Trade(ticker: Ticker, quantity: Int, price: BigDecimal)

    object Rates {
      implicit val M: Monoid[Rates] = Monoid.instance((o1, o2) => Rates(o1.rs ::: o2.rs), Rates(Nil))
    }
    case class Rates(rs: List[(Currency, Currency, BigDecimal)]) {
      def verifiedUSD: Program[Map[Currency, BigDecimal]] = Program.either((\/.right[E, Map[Currency, BigDecimal]](Map.empty) /: (rs.toStream collect { case (c, Currency.USD, r) => c -> r})) { case (d, p @ (c, r)) =>
        d.flatMap(rs => rs.get(c).fold(\/.right[E, Map[Currency, BigDecimal]](rs + p))( rr => if (r == rr) d else \/.left(MismatchedRates(c, rr, r))))
      })

      def +(r: (Currency, Currency, BigDecimal)) = copy(rs = r :: this.rs)
    }

    object OfficialRates {
      implicit val M: Monoid[OfficialRates] = Monoid.instance((o1, o2) => OfficialRates(o1.rates ++ o2.rates), OfficialRates(Map.empty))
    }
    case class OfficialRates(rates: Map[(Currency, Currency), BigDecimal]) {
      def +(pair: (Currency, Currency), rate: BigDecimal) = copy(rates = this.rates + (pair -> rate))
    }

    case class Config(pathToRates: String, pathToOfficial: String)

    type M[A] = ReaderWriterStateT[effect.IO, Config, Unit, Unit, A]
    type ProgramError[E, A] = EitherT[M, E, A]
    type Program[A] = ProgramError[E, A]
    object Program {

      def io[A](f: Config => effect.IO[E \/ A]): Program[A] = EitherT.eitherT[M, E, A](RWST[effect.IO, Config, Unit, Unit, E \/ A]((config, s) => f(config).map(x => ((), x, s))))
      def apply[A](f: Config => E \/ A): Program[A] = io(f andThen (_.point[effect.IO]))

      def either[A](e: E \/ A): Program[A] = apply(_ => e)
      def eitherIO[A](e: effect.IO[E \/ A]): Program[A] = io(_ => e)

      def unit[A](a: => A): Program[A] = either(\/.right(a))
      def fail[A](e: E): Program[A] = either(\/.left(e))

      def reads[A](f: Config => A): Program[A] = apply(f andThen \/.right)

      def tell(w: => Unit): Program[Unit] = EitherT.right[M, E, Unit](RWST[effect.IO, Config, Unit, Unit, Unit]((config, s) => effect.IO((w, (), ()))))


      def monadErrorThrowable = EitherT.eitherTMonadError[M, Throwable]
      def monadCatchIOThrowable = IOUtil.eitherTMonadCatchIO[({type l[a]=RWST[effect.IO, Config, Unit, Unit, a]})#l, Throwable](IOUtil.rwstMonadCatchIO[effect.IO, Config, Unit, Unit])

      implicit def monadError = EitherT.eitherTMonadError[M, E]
      implicit def monadCatchIO = IOUtil.eitherTMonadCatchIO[({type l[a]=RWST[effect.IO, Config, Unit, Unit, a]})#l, E](IOUtil.rwstMonadCatchIO[effect.IO, Config, Unit, Unit])
    }

    def officialRates: Program[OfficialRates] = {
        for {
          s <- Program.reads(_.pathToOfficial)
          p <- Program.either(Option(getClass.getResource(s)).toRightDisjunction[E](NoSuchResource(s)))
          ls <- IOUtil.readAllLines1[({type l[a, b] = EitherT[M, a, b]})#l](Paths get p.toURI)(Program.monadErrorThrowable).leftMap(ioException)
          csv <- toCsv(ls.toStream)                                                          // ^^^^^^^^^^^^^^^^^^^^^^^^^^
          x <- csv parseZero { indices => line =>
                State.modify[OfficialRates](o => {
                  val cells = line.split(",")
                  o +((Currency.valueOf(cells(indices("Currency1"))), Currency.valueOf(cells(indices("Currency2")))), BigDecimal(cells(indices("Rate"))))
                })
              }
          (a, b) = x }
          yield a
    }

    case class Csv(indices: Map[String, Int], rows: Stream[String]) {
      def parse[S, A](processRow: Map[String, Int] => String => State[S, A])(s: S): Program[(S, List[A])] = {

        Program.either( \/.fromTryCatchNonFatal(rows.toList.traverseU(processRow(indices)).run(s)).leftMap(ParseException) )
      }
      def parseZero[S: Monoid, A](processRow: Map[String, Int] => String => State[S, A]): Program[(S, List[A])] = parse(processRow)(Monoid[S].zero)
    }

    def toCsv(lines: Stream[String]): Program[Csv] =
      lines match {
        case Stream.Empty    => Program.fail(NoHeader)
        case header #:: data => Program.unit(Csv(header.split(",").toStream.map(_.trim).zipWithIndex.toMap, data))
      }

    def brokerRates: Program[Rates] =
      for  {
        s <- Program.reads(_.pathToRates)
        p <- Program.either(Option(getClass.getResource(s)).toRightDisjunction[E](NoSuchResource(s)))
        ls <- IOUtil.readAllLines3[({type l[a, b] = EitherT[M, a, b]})#l](Paths get p.toURI)(Program.monadErrorThrowable, Program.monadCatchIOThrowable).leftMap(ioException)
        csv <- toCsv(ls.toStream)                                                         // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        x <- csv parseZero { indices => line =>
          State.modify[Rates](rs => {
            val cells = line.split(",")
            rs + (Currency.valueOf(cells(indices("TradedCurrency"))), Currency.USD, BigDecimal(cells(indices("Rate (USD)"))))
          })
        }
        (a, b) = x
      } yield a

    def brokerRates2: Program[Rates] = {
      import Program._
      for {
        s <- Program.reads(_.pathToRates)
        p <- Program.either(Option(this.getClass.getResource(s)).toRightDisjunction[E](NoSuchResource(s)))
        ls <- IOUtil.readAllLines[ProgramError, E](Paths get p.toURI)(ioException)
        csv <- toCsv(ls.toStream)
        x <- csv parseZero { indices => line =>
          State.modify[Rates](rs => {
            val cells = line.split(",")
            rs +(Currency.valueOf(cells(indices("TradedCurrency"))), Currency.USD, BigDecimal(cells(indices("Rate (USD)"))))
          })
        }
        (a, b) = x
      } yield a
    }

    def sanitized(o: OfficialRates, b: Map[Currency, BigDecimal]) = b

    val rates =
      for {
        o <- officialRates
        _ <- Program.tell(info(s"Official rates are $o"))
        b <- brokerRates2
        _ <- Program.tell(info(s"Broker rates are $b"))
        v <- b.verifiedUSD
        _ <- Program.tell(info(s"Verified rates are $v"))
      } yield sanitized(o, v)

}
