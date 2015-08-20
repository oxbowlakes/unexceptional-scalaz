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
      a <- Csv.parseExecZero[Throws, E, OfficialRates, Unit](ls.toStream)(parseException) { cells =>
                State.modify[OfficialRates](_ +((Currency.valueOf(cells("Currency1")), Currency.valueOf(cells("Currency2"))), BigDecimal(cells("Rate"))))
      }
    }
    yield a
  }



  def brokerRates2: Returns[Rates] = {
    for {
      s <- reads(_.pathToRates)
      p <- either(Option(this.getClass.getResource(s)).toRightDisjunction[E](NoSuchResource(s)))
      a <- Csv.parseExecZeroIO[Throws, E, Rates, Unit](Paths get p.toURI)({
              case e: java.io.IOException => ioException(e)
              case t: Throwable           => parseException(t)
           }) { cells =>
              State.modify[Rates](_ + (Currency.valueOf(cells("TradedCurrency")), Currency.USD, BigDecimal(cells("Rate (USD)"))))
            }
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
