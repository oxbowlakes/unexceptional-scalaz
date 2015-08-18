package oxbow.support

trait UnderlyingThrowable {
  def underlying: Throwable

  override final def toString = {
    import java.io._
    val w = new StringWriter
    underlying.printStackTrace(new PrintWriter(w))
    super.toString  ++ "\\n" ++ w.toString
  }
}
