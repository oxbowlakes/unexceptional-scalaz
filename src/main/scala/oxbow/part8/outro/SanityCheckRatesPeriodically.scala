package oxbow.part8.outro

import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Executors, TimeUnit}

import oxbow.support.{Currency, Logged, UnderlyingThrowable}

import scalaz.Leibniz.===
import scalaz.Scalaz._
import scalaz._

object SanityCheckRatesPeriodically extends App with Logged {
    sealed trait E
    case class NoSuchResource(path: String) extends E
    case class IOException(underlying: Throwable) extends E with UnderlyingThrowable
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
    type Program[A] = EitherT[M, E, A]
    object Program {

      def io[A](f: Config => effect.IO[E \/ A]): Program[A] = EitherT.eitherT[M, E, A](RWST[effect.IO, Config, Unit, Unit, E \/ A]((config, s) => f(config).map(x => ((), x, s))))
      def apply[A](f: Config => E \/ A): Program[A] = io(f andThen (_.point[effect.IO]))

      def either[A](e: E \/ A): Program[A] = apply(_ => e)
      def eitherIO[A](e: effect.IO[E \/ A]): Program[A] = io(_ => e)

      def unit[A](a: => A): Program[A] = either(\/.right(a))
      def fail[A](e: E): Program[A] = either(\/.left(e))

      def reads[A](f: Config => A): Program[A] = apply(f andThen \/.right)

      def tell(w: => Unit): Program[Unit] = EitherT.right[M, E, Unit](RWST[effect.IO, Config, Unit, Unit, Unit]((config, s) => effect.IO((w, (), ()))))
    }

    def officialRates: Program[OfficialRates] = {
        for (p <- Program.reads(_.pathToOfficial); ls <- readAllLines(p); csv <- toCsv(ls); x <- csv parseZero { indices => line =>
          State.modify[OfficialRates](o => {
            val cells = line.split(",")
            o +((Currency.valueOf(cells(indices("Currency1"))), Currency.valueOf(cells(indices("Currency2")))), BigDecimal(cells(indices("Rate"))))
          })
        }; (a, b) = x) yield a
    }

    import collection.JavaConverters._
    def readAllLines(pathToRates: String): Program[Stream[String]]
     = Program.eitherIO {
      effect.IO {
        for (r <- Option(getClass.getResource(pathToRates)).toRightDisjunction[E](NoSuchResource(pathToRates)); x <- \/.fromTryCatchNonFatal(Files.readAllLines(Paths.get(r.toURI))).leftMap(IOException)) yield x
      }
    } map (_.asScala.toStream)

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
      for (p <- Program.reads(_.pathToRates); ls <- readAllLines(p); csv <- toCsv(ls); x <- csv parseZero { indices => line =>
      State.modify[Rates](rs => {
        val cells = line.split(",")
        rs + (Currency.valueOf(cells(indices("TradedCurrency"))), Currency.USD, BigDecimal(cells(indices("Rate (USD)"))))
      })
    }; (a, b) = x) yield a

    def sanitized(o: OfficialRates, b: Map[Currency, BigDecimal]) = b //for simplicity, for now

    val rates =
      for {
        o <- officialRates
        _ <- Program.tell(info(s"Official rates are $o"))
        b <- brokerRates
        _ <- Program.tell(info(s"Broker rates are $b"))
        v <- b.verifiedUSD
        _ <- Program.tell(info(s"Verified rates are $v"))
      } yield sanitized(o, v)


    val cfg = Config("/mnt/live/rates/deutsche/2015/09/trades-20150919.csv", "/mnt/live/rates/deutsche/2015/09/official-20150918.csv")

    val state = new AtomicReference[Map[Currency, BigDecimal]](Map.empty[Currency, BigDecimal])


    Executors.newSingleThreadScheduledExecutor().scheduleAtFixedRate(new Runnable {
      override def run() = {
        type ST[A] = StateT[effect.IO, Map[Currency, BigDecimal], A]
        val et: EitherT[ST, E, Unit] =
          EitherT[ST, E, Unit](StateT[effect.IO, Map[Currency, BigDecimal], E \/ Unit] { s => {
            rates.run.run(cfg, ()) map {
              case (_, -\/(e), _) => (Map.empty[Currency, BigDecimal], -\/(e))
              case (_, \/-(a), _) => (a, \/-(()))
            }
          }})

        val update =
          state.testAndSet[effect.IO, E, Unit](m => effect.IO { m.isEmpty })(et)
        update.run.unsafePerformIO().valueOr(e => warning(e.toString))
      }
    }, 0, 15, TimeUnit.MINUTES)

  //What have we done here?
  //
  // 1. We have a single piece of mutable state, protected in an atomic reference
  // 2. We compose a program as a state transition which can be applied to that state
  //     You might envisage that you are running an HTTP server and each request maps to some state-transition action
  //     You could atomically attempt the action and serve up the response


  Executors.newSingleThreadScheduledExecutor().scheduleAtFixedRate(new Runnable {
    override def run() = {
      type ST[A] = StateT[effect.IO, Map[Currency, BigDecimal], A]
      val et: EitherT[ST, E, Unit] = {
        //EitherT with the error type fixed as our E. This satisfies the kind required by Hoist/MonadTrans
        type ET[F[_], A] = EitherT[F, E, A]

        //StateT with Unit as the state type. We use this as the type of the intermediate step in which we have transformed `rates` to use StateT
        //instead of RWST
        type ST_[A] = StateT[effect.IO, Unit, A]

        //This lets us turn any M[A] into an ST_[A], for any A
        val rwstToStateTUnit: M ~> ST_ = new (M ~> ST_) {
          override def apply[A](fa: M[A]): ST_[A] = fa.state(cfg)
        }

        //If we have a Lens[S1, S2] then we can turn a StateT[F, S2, A] into a StateT[F, S1, A].
        //You can think of this as the 'smaller' state (S2) function operating on the S2 part of the 'larger' state (S1), and then updating the
        //S2 part of the S1 originally passed to it when it is finished. To turn an ST_ into an ST, we need a Lens[Map[Currency, BigDecimal], Unit].
        val stateTUnitToStateTS: ST_ ~> ST = new (ST_ ~> ST) {
          override def apply[A](fa: ST_[A]): ST[A] = fa.zoom(LensFamily.trivialLens)
        }

        //We can compose natural transformations. This lets us turn any M[A] into an ST[A], for any A
        val rwstToStateTS: M ~> ST = stateTUnitToStateTS.compose(rwstToStateTUnit)

        //We can alter the monad inside a monad transformer using a Hoist, if we have a natural transformation from the original inner monad to
        //the resulting inner monad. We can use this function to turn `rates` (which is an ET[M, A]) into an ET[ST, A], which is the shape we
        //need for the result.
        val transform: NaturalTransformation[({type l[a]=ET[M, a]})#l, ({type l[a]=ET[ST, a]})#l] = Hoist[ET].hoist[M, ST](rwstToStateTS)

        //We also need to update the state. We can get a ST[Unit] that does this, but we need to lift it into an ET[ST, Unit].
        //MonadTrans[ET] will let us do this. It can turn any N[A] into an ET[N, A], if N is a monad
        val M = StateT.stateTMonadState[Map[Currency, BigDecimal], effect.IO]
        def update(state: Map[Currency, BigDecimal]): ET[ST, Unit] = MonadTrans[ET].liftMU(M.put(state))

        for {
          state <- transform(rates)
          _ <- update(state)
        } yield ()
      }

      val update =
        state.testAndSet[effect.IO, E, Unit](m => effect.IO { m.isEmpty })(et)
      update.run.unsafePerformIO().valueOr(e => warning(e.toString))
    }
  }, 0, 15, TimeUnit.MINUTES)

}
