package oxbow.part999

import java.nio.file.Paths
import java.util.logging.Level

import oxbow.support.Currency

import scalaz._
import Scalaz._


object SanityCheckRates extends Program[Config, Unit, E] with App {

  def officialRates: Returns[OfficialRates] = {
    for {
      s <- reads(_.pathToOfficial)
      p <- either(Option(this.getClass.getResource(s)).toRightDisjunction[E](NoSuchResource(s)))
      ls <- IOUtil.readAllLines[Throws, E](Paths get p.toURI)(ioException)
      csv <- toCsv(ls.toStream)
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
    def parse[S, A](processRow: Map[String, Int] => String => State[S, A])(s: S): Returns[(S, List[A])] = {

      either( \/.fromTryCatchNonFatal(rows.toList.traverseU(processRow(indices)).run(s)).leftMap(ParseException) )
    }
    def parseZero[S: Monoid, A](processRow: Map[String, Int] => String => State[S, A]): Returns[(S, List[A])] = parse(processRow)(Monoid[S].zero)
  }

  def toCsv(lines: Stream[String]): Returns[Csv] =
    lines match {
      case Stream.Empty    => fail(NoHeader)
      case header #:: data => unit(Csv(header.split(",").toStream.map(_.trim).zipWithIndex.toMap, data))
    }

  def brokerRates2: Returns[Rates] = {
    for {
      s <- reads(_.pathToRates)
      p <- either(Option(this.getClass.getResource(s)).toRightDisjunction[E](NoSuchResource(s)))
      ls <- IOUtil.readAllLines[Throws, E](Paths get p.toURI)(ioException)
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
      _ <- tell(s"Official rates are $o", Level.INFO)
      b <- brokerRates2
      _ <- tell(s"Broker rates are $b", Level.INFO)
      v <- either((\/.right[E, Map[Currency, BigDecimal]](Map.empty) /: (b.rs.toStream collect { case (c, Currency.USD, r) => c -> r})) { case (d, p @ (c, r)) =>
            d.flatMap(rs => rs.get(c).fold(\/.right[E, Map[Currency, BigDecimal]](rs + p))( rr => if (r == rr) d else \/.left(MismatchedRates(c, rr, r))))
          })
    } yield sanitized(o, v)


  val cfg = Config("/mnt/live/rates/deutsche/2015/09/trades-20150919.csv", "/mnt/live/rates/deutsche/2015/09/official-20150918.csv")
  println( evalZeroUnsafePerformIO(rates)(cfg) )


}
