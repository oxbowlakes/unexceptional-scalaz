package oxbow.part2.exceptions.disjunction1

import java.io._
import java.nio.file.{Files, Paths}

import scalaz._

object ScalazDisjunction1b extends App {
  private val pathToReader: String = "/path/to/file"

  def example1(): Unit = {
    //Note out code has no exception handling
    findReader().map(slurpReader).foreach(System.out.println)
  }

  private def slurpReader(r: java.io.Reader): Option[String] = {
    val b = new BufferedReader(r)
    try {
      def nextLine() = Option(b.readLine())

      val buf = new StringBuilder
      var l = Option.empty[String]
      do {
        l = nextLine()
        l.foreach(buf ++= _)
      } while (l.isDefined)
      Some(buf.toString())
    }
    catch { case _ : IOException  => None }
    finally b.close()
  }

  private def findReader(): FileNotFoundException \/ java.io.Reader = {
    //Note you are forced to think about what your failure types should be
    // 1. A String? (not very typesafe!)
    // 2. An Exception (not actually that useful in practice)
    // 3. A sealed trait ( \o/ )

    \/.fromTryCatchNonFatal(new FileReader(pathToReader)).leftMap(t => new FileNotFoundException(s"$pathToReader: ${t.getMessage}"))
  }

  /**
   * We decide to 'refactor' (OK, change) to using <tt>java.nio.file</tt> API and construct
   * a reader using <tt>Files.newBufferedReader</tt>. The Java compiler helps us by indicating that
   * this throws a different exception type
   */
  def example2(): Unit = {
      findReaderNio2().map(slurpReader).foreach(System.out.println)
  }

  private def findReaderNio2(): FileNotFoundException \/ java.io.Reader = {
    \/.fromTryCatchNonFatal(Files.newBufferedReader(Paths.get(pathToReader))).leftMap(t => new FileNotFoundException(s"$pathToReader: ${t.getMessage}"))
  }

}
