package oxbow.part2.exceptions.disjunction1

import java.io._
import java.nio.file.{Files, Paths}

import scalaz._

object ScalazDisjunction1a extends App {
  private val pathToReader: String = "/path/to/file"

  def example1(): Unit = {
    //Note out code has no exception handling and we can use map and foreach
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
    try \/.right(new FileReader(pathToReader)) catch { case e: FileNotFoundException => \/.left(e)}
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
    //Oh dear, this is still borked. Compiler is not helping me notice that this exception is not thrown :-(
    try \/.right(Files.newBufferedReader(Paths.get(pathToReader))) catch { case e: FileNotFoundException => \/.left(e)}
  }

}
