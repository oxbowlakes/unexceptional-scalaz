package oxbow.part1.intro

import java.io._
import java.nio.file.{Files, Paths}

class ScalaUncheckedExceptions {
  private val pathToReader: String = "/path/to/file"

  def example1(): Unit = {
    try {
      val r = findReader()
      slurpReader(r).foreach(System.out.println)
    }
    catch {
      case e: FileNotFoundException =>
        System.out.println("No reader found")
    }
  }

  private def slurpReader(r: Reader): Option[String] = {
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

  @throws(classOf[FileNotFoundException]) //Not required of course
  private def findReader(): Reader = {
    new FileReader(pathToReader)
  }

  /**
   * We decide to 'refactor' (OK, change) to using <tt>java.nio.file</tt> API and construct
   * a reader using <tt>Files.newBufferedReader</tt>. The Java compiler helps us by indicating that
   * this throws a different exception type
   */
  def example2(): Unit = {
    try {
      val r: Reader = findReaderNio2
      slurpReader(r).foreach(System.out.println)
    }
    catch {
      case e: FileNotFoundException => //catch block is never hit; I have just broken my program and the compiler didn't help me
        System.out.println("No reader found")
    }
  }

  @throws(classOf[FileNotFoundException]) //compiles, even though it does not throw one of these :-(
  private def findReaderNio2: Reader = {
    Files.newBufferedReader(Paths.get(pathToReader)) //compiles just fine
  }
}
