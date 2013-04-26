package net.szumo.fstl

trait CaseType {
  def variants(c: Char): Seq[Char]
  def canonical(c: Char): Char
}

object CaseType {
  def apply(caseSensitive: Boolean):CaseType = if (caseSensitive) CaseSensitive else CaseInsensitive
}

object CaseSensitive extends CaseType {
  def variants(c: Char): Seq[Char] = Seq(c)
  def canonical(c: Char): Char = c

}
object CaseInsensitive extends CaseType {
  def variants(c: Char): Seq[Char] = if (c.isLower || c.isUpper) Seq(c.toUpper, c.toLower) else Seq(c)
  def canonical(c: Char): Char = c.toLower
}
