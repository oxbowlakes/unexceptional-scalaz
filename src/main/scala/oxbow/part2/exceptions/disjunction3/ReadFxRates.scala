package oxbow.part2.exceptions.disjunction3

import java.nio.file.{Paths, Files}

import oxbow.support.Currency

import scalaz._; import Scalaz._

object ReadFxRates extends App {

  sealed trait E
  case class IOException(underlying: Throwable) extends E
  case object NoHeader extends E
  case class ParseException(t: Throwable) extends E
  case class MismatchedRates(ccy: Currency, r1: BigDecimal, r2: BigDecimal) extends E

  case class Rates(rs: List[(Currency, Currency, BigDecimal)]) {
    def verifiedUSD: E \/ Map[Currency, BigDecimal] = (\/.right[E, Map[Currency, BigDecimal]](Map.empty) /: (rs.toStream collect { case (c, Currency.USD, r) => c -> r})) { case (d, p @ (c, r)) =>
        //If fail, fail
        //If success and c is present with different r, fail
        //Else success and add c -> r
        d.flatMap(rs => rs.get(c).fold(\/.right[E, Map[Currency, BigDecimal]](rs + p))( rr => if (r == rr) d else \/.left(MismatchedRates(c, rr, r))))
    }
  }

  val pathToRates = "/mnt/live/rates/deutsche/2015/09/trades-20150919.csv"

  //Say the file is "Ticker, TradedCurrency, Rate (USD)"

  def parse(indices: Map[String, Int], rows: Stream[String]): E \/ Rates = {
    def rate(line: String): E \/ (Currency, Currency, BigDecimal) = {
      val cells = line.split(",")
      //Could be more detailed here of course
      \/.fromTryCatchNonFatal(Currency.valueOf(cells(indices("TradedCurrency"))), Currency.USD, BigDecimal(cells(indices("Rate (USD)")))).leftMap(ParseException)
    }
    rows.map(rate).sequenceU.map(s => Rates(s.toList))  //IDEA unhappy :-( Note: no error accumulation, this is fail-fast
  }


  import collection.JavaConverters._
  //Oh dear; we discover this is a Java call and is unsafe
  /*
  Files.readAllLines(Paths.get(pathToRates)).asScala.toStream match {
    case header #:: data => val is: Map[String, Int] = header.split(",").zipWithIndex.toMap; parse(is, data)
    case Stream.Empty    => \/.left(NoHeader)
  }
  */

  def readAllLines: E \/ Stream[String] = \/.fromTryCatchNonFatal(Files.readAllLines(Paths.get(getClass.getResource(pathToRates).toURI))).bimap(IOException, _.asScala.toStream)

  case class Csv(indices: Map[String, Int], rows: Stream[String]) {
    def parse: E \/ Rates = {
      def rate(line: String): E \/ (Currency, Currency, BigDecimal) = {
        val cells = line.split(",")
        //Could be more detailed here of course
        \/.fromTryCatchNonFatal(Currency.valueOf(cells(indices("TradedCurrency"))), Currency.USD, BigDecimal(cells(indices("Rate (USD)")))).leftMap(ParseException)
      }
      //Note (xs map f).sequence is just xs.traverse(f)
      rows.map(rate).sequenceU.map(s => Rates(s.toList))  //IDEA unhappy :-( Note: no error accumulation, this is fail-fast
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
      rs <- f.parse
      v <- rs.verifiedUSD
    } yield v

  println(rates)
}
