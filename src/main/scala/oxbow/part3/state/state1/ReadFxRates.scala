package oxbow.part3.state.state1

import java.nio.file.{Paths, Files}

import oxbow.support.Currency

import scalaz._; import Scalaz._

object ReadFxRates extends App {

  sealed trait E
  case class IOException(underlying: Throwable) extends E
  case object NoHeader extends E
  case class ParseException(t: Throwable) extends E
  case class MismatchedRates(ccy: Currency, r1: BigDecimal, r2: BigDecimal) extends E

  class Ticker(val symbol: String) extends AnyVal
  case class Trade(ticker: Ticker, quantity: Int, price: BigDecimal)
  case class Rates(rs: List[(Currency, Currency, BigDecimal)]) {
    def verifiedUSD: E \/ Map[Currency, BigDecimal] = (\/.right[E, Map[Currency, BigDecimal]](Map.empty) /: (rs.toStream collect { case (c, Currency.USD, r) => c -> r})) { case (d, p @ (c, r)) =>
      d.flatMap(rs => rs.get(c).fold(\/.right[E, Map[Currency, BigDecimal]](rs + p))( rr => if (r == rr) d else \/.left(MismatchedRates(c, rr, r))))
    }

    def +(r: (Currency, Currency, BigDecimal)) = copy(rs = r :: this.rs)
  }

  val pathToRates = "/mnt/live/rates/deutsche/2015/09/trades-20150919.csv"



  import collection.JavaConverters._
  def readAllLines: E \/ Stream[String] = \/.fromTryCatchNonFatal(Files.readAllLines(Paths.get(getClass.getResource(pathToRates).toURI))).bimap(IOException, _.asScala.toStream)

  case class Csv(indices: Map[String, Int], rows: Stream[String]) {
    def parse: E \/ (Rates, List[Trade]) = {

      /* Example of a State transition */
      def processRow(line: String) = State[Rates, Trade] { rates =>
        val cells = line.split(",")
        //Could be more detailed here of course
        (rates + (Currency.valueOf(cells(indices("TradedCurrency"))), Currency.USD, BigDecimal(cells(indices("Rate (USD)")))), Trade(new Ticker(cells(indices("Ticker"))), cells(indices("Quantity")).toInt, BigDecimal(cells(indices("Price")))))
      }

      \/.fromTryCatchNonFatal(rows.toList.traverseU(processRow).run(Rates(Nil))).leftMap(ParseException)
    }
  }

  def toCsv(lines: Stream[String]): E \/ Csv =
    lines match {
      case Stream.Empty    => \/.left(NoHeader)
      case header #:: data => \/.right(Csv(header.split(",").toStream.map(_.trim).zipWithIndex.toMap, data))
    }

  //Then ...
  val rates =
    for {
      lines <- readAllLines
      f <- toCsv(lines)
      y <- f.parse
      (rs, ts) = y
      v <- rs.verifiedUSD
    } yield (v, ts)

  println(rates)
}
