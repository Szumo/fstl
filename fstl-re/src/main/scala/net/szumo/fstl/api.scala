package net.szumo.fstl

trait FSTLRegex {
  def | (other: FSTLRegex): FSTLRegex
  def & (other: FSTLRegex): FSTLRegex
  def unary_^ (other: FSTLRegex): FSTLRegex
  def apply(text: Iterator[Char]): Boolean
  def apply(text: String): Boolean
  def pattern: String
}

object FSTLRegex {
  import java.util.regex.{Pattern => JavaPattern}
  private final val RequiredFlags:Map[Int, String] = Map.empty
  private final val ForbiddenFlags:Map[Int, String] = Map.empty

  def apply(string: String, caseSensitive: Boolean): FSTLRegex = null
  def apply(strings: Iterable[String], caseSensitive: Boolean): FSTLRegex = null
  def apply(string: String): FSTLRegex = apply(string, true)
  def apply(strings: Iterable[String]): FSTLRegex = apply(strings, true)
  def apply(regex: scala.util.matching.Regex): FSTLRegex = apply(regex.pattern)
  def apply(pattern: JavaPattern):FSTLRegex = {
    val flags = pattern.flags()
    for ( (required, message) <- RequiredFlags ) require( (flags & required) != 0, message)
    for ( (forbidden, message) <- ForbiddenFlags) require ( (flags & forbidden) == 0, message)
    apply(pattern.pattern(), (flags & JavaPattern.CASE_INSENSITIVE) == 0)
  }

  implicit class String2Regex(val s:String) extends AnyVal {
    def fr: FSTLRegex = FSTLRegex(s)
  }
}

