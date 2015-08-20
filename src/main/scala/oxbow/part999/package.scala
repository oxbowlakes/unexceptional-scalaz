package oxbow

import oxbow.support.{Currency, UnderlyingThrowable}

import scalaz._; import Scalaz._
package object part999 {
  sealed trait E
  case class NoSuchResource(path: String) extends E
  case class IOException(underlying: Throwable) extends E with UnderlyingThrowable
  def ioException(t: Throwable): E = IOException(t)
  case class ParseException(underlying: Throwable) extends E with UnderlyingThrowable
  def parseException(t: Throwable): E = ParseException(t)
  case class MismatchedRates(ccy: Currency, r1: BigDecimal, r2: BigDecimal) extends E

  class Ticker(val symbol: String) extends AnyVal
  case class Trade(ticker: Ticker, quantity: Int, price: BigDecimal)

  object Rates {
    implicit val M: Monoid[Rates] = Monoid.instance((o1, o2) => Rates(o1.rs ::: o2.rs), Rates(Nil))
  }
  case class Rates(rs: List[(Currency, Currency, BigDecimal)]) {

    def +(r: (Currency, Currency, BigDecimal)) = copy(rs = r :: this.rs)
  }

  object OfficialRates {
    implicit val M: Monoid[OfficialRates] = Monoid.instance((o1, o2) => OfficialRates(o1.rates ++ o2.rates), OfficialRates(Map.empty))
  }
  case class OfficialRates(rates: Map[(Currency, Currency), BigDecimal]) {
    def +(pair: (Currency, Currency), rate: BigDecimal) = copy(rates = this.rates + (pair -> rate))
  }

  case class Config(pathToRates: String, pathToOfficial: String)

}
