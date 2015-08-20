package oxbow.part2.exceptions.disjunction2

import java.nio.file.{Paths, Files}

import scalaz._; import Scalaz._
object ScalazDisjunction3 extends App {

  sealed trait E
  case class MissingProperty(k: String) extends E
  case class MissingArg(k: String) extends E
  case class CannotParse(v: String, msg: String) extends E

  def systemProperty(k: String): E \/ String = Option(System.getProperty(k)).toRightDisjunction(MissingProperty(k))
  def programArg(k: String): E \/ String = (args.toSeq sliding 2 collectFirst { case Seq(k, s) => s }).toRightDisjunction(MissingArg(k))

  lazy val defaultPoolSize = Runtime.getRuntime.availableProcessors()
  val poolSizeFromSysProp = for (s <- systemProperty("threadPool.size"); i <- s.parseInt.disjunction.leftMap(t => CannotParse(s, t.getMessage))) yield i
  val poolSize = poolSizeFromSysProp | defaultPoolSize //getOrElse
//  val poolSize2 = poolSizeFromSysProp valueOr { e => sys.error(s"Bah! $e")}

  def poolSizeFromArgs = for (p <- programArg("--pool-size"); i <- p.parseInt.disjunction.leftMap(t => CannotParse(p, t.getMessage))) yield i

  val poolSize3 = poolSizeFromSysProp orElse poolSizeFromArgs getOrElse defaultPoolSize
  val poolSize4 = poolSizeFromSysProp ||| poolSizeFromArgs | defaultPoolSize


  val fileContents: effect.IO[E \/ java.util.List[String]] = systemProperty("config.file").traverse(s => effect.IO { Files.readAllLines(Paths get s)})
                                                                                                                    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ this might fail

  case class IOException(underlying: Throwable) extends E
  def ioException(t: Throwable): E = IOException(t)
  type ET[A] = EitherT[effect.IO, E, A]
  val lines: ET[java.util.List[String]] =
    for {
      s <- EitherT(effect.IO( systemProperty("config.file")))
      ls <- EitherT(effect.IO { \/.fromTryCatchNonFatal(Files.readAllLines(Paths get s)).leftMap(ioException)})
    } yield ls                                                                               //  ^^^^^^^^^^^

  val lines1: ET[java.util.List[String]] =
    for {
      s <- EitherT(effect.IO( systemProperty("config.file")))
      ls <- EitherT(effect.IO { \/.fromTryCatchNonFatal(Files.readAllLines(Paths get s)).leftMap(ioException)})
    } yield ls

  val lines2: ET[java.util.List[String]] =
    for {
      s <- EitherT(effect.IO( systemProperty("config.file")))
      ls <- EitherT(effect.IO { \/.fromTryCatchNonFatal(Files.readAllLines(Paths get getClass.getResource(s).toURI)).leftMap(ioException)})
    } yield ls                                                                     //^^^^^^^^^^^^^^^^^^^^^^^ may be null

  //get IO[E \/ List[String]]
  println( lines.run.unsafePerformIO() )
  System.setProperty("config.file", "/mnt/live/rates/deutsche/2015/09/trades-20150919.csv")
  println( lines1.run.unsafePerformIO() )
  println( lines2.run.unsafePerformIO() )

  System.clearProperty("config.file")
  println()
  println()
  //Of course, this is just:
  println( (for {
    _ <- lines
    _ <- EitherT.right(effect.IO(System.setProperty("config.file", "/mnt/live/rates/deutsche/2015/09/trades-20150919.csv")))
    _ <- lines1
    _ <- lines2
  } yield ()).run.unsafePerformIO() )

  //Except it isn't - sequence!

  implicit val ShowE: Show[E] = Show.showFromToString
  implicit def ShowJavaList[A: Show]: Show[java.util.List[A]] = Show.showFromToString
  System.clearProperty("config.file")

  println()
  println()
  List(
    lines.run.map(_.shows) flatMap effect.IO.putStrLn,
    effect.IO(System.setProperty("config.file", "/mnt/live/rates/deutsche/2015/09/trades-20150919.csv")) >> effect.IO.ioUnit,
    lines1.run.map(_.shows) flatMap effect.IO.putStrLn,
    lines2.run.map(_.shows) flatMap effect.IO.putStrLn).sequence.unsafePerformIO()

  System.clearProperty("config.file")

  println()
  println()
  case class MissingResource(p: String) extends E
  def resource(s: String): E \/ java.net.URI = Option(getClass.getResource(s)).toRightDisjunction(MissingResource(s)).map(_.toURI)
  val lines3: ET[java.util.List[String]] =
    for {
      s <- EitherT(effect.IO( systemProperty("config.file")))
      ls <- EitherT(effect.IO { for (r <- resource(s); ls <- \/.fromTryCatchNonFatal(Files.readAllLines(Paths get r)).leftMap(ioException)) yield ls})
    } yield ls                                                                     
  (for {
    _ <- effect.IO(System.setProperty("config.file", "/mnt/live/rates/deutsche/2015/09/trades-20150918.csv"))
    l2a <- lines3.run.map(_.shows)
    _ <- effect.IO.putStrLn(l2a)
    _ <- effect.IO(System.setProperty("config.file", "/mnt/live/rates/deutsche/2015/09/trades-20150919.csv"))
    l2b <- lines3.run.map(_.shows)
    _ <- effect.IO.putStrLn(l2b)
  } yield ()).unsafePerformIO()
}
