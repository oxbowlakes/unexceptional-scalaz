package oxbow.support

import java.util.logging.Level

trait Logged {
  protected lazy val logger = java.util.logging.Logger.getLogger(getClass.getName)

  def severe(s: => String) = if (logger.isLoggable(Level.SEVERE)) logger.log(Level.SEVERE, s)
  def severe(t: Throwable, s: => String) = if (logger.isLoggable(Level.SEVERE)) logger.log(Level.SEVERE, s, t)

  def warning(s: => String) = if (logger.isLoggable(Level.WARNING)) logger.log(Level.WARNING, s)
  def warning(t: Throwable, s: => String) = if (logger.isLoggable(Level.WARNING)) logger.log(Level.WARNING, s, t)

  def info(s: => String) = if (logger.isLoggable(Level.INFO)) logger.log(Level.INFO, s)
  def info(t: Throwable, s: => String) = if (logger.isLoggable(Level.INFO)) logger.log(Level.INFO, s, t)

  def fine(s: => String) = if (logger.isLoggable(Level.FINE)) logger.log(Level.FINE, s)
  def fine(t: Throwable, s: => String) = if (logger.isLoggable(Level.FINE)) logger.log(Level.FINE, s, t)

  def finer(s: => String) = if (logger.isLoggable(Level.FINER)) logger.log(Level.FINER, s)
  def finer(t: Throwable, s: => String) = if (logger.isLoggable(Level.FINER)) logger.log(Level.FINER, s, t)

  def finest(s: => String) = if (logger.isLoggable(Level.FINEST)) logger.log(Level.FINEST, s)
  def finest(t: Throwable, s: => String) = if (logger.isLoggable(Level.FINEST)) logger.log(Level.FINEST, s, t)
}
