package net.szumo.fstl

// StringMatcher is used to look for many words inside a string in one pass (using Aho-Corasick algorithm)
// StringMatcher maps words to outputs.
// ContextStringMatcher maps them to match positions.

trait CaseType {
  def addEquivalents[V](m: Map[Char,V], c: Char, v: V): Map[Char, V]
}
object CaseSensitive extends CaseType {
  def addEquivalents[V](m: Map[Char,V], c: Char, v: V): Map[Char, V] = m.updated(c, v)
}
object CaseInsensitive extends CaseType {
  def addEquivalents[V](m: Map[Char,V], c: Char, v: V): Map[Char, V] = if (c.isLower || c.isUpper) m.updated(c.toUpper, v).updated(c.toLower, v) else m.updated(c, v)
}

case class MatchPosition(word:String, start: Int, stop:Int)

trait StringMatcher[T] {
  def isMatch(s: String) : Boolean
  def isMatch(s: Iterable[String]) : Boolean
  def apply(s: String): Iterator[T]
  def apply(s: Iterable[String]): Iterator[T]
}

object StringMatcher {
  def caseType(caseSensitive: Boolean):CaseType = if (caseSensitive) CaseSensitive else CaseInsensitive
  def apply(words: Iterable[String], caseType: CaseType):StringMatcher[String] = apply(words, caseType, identity _)
  def apppy[T](words: Map[String, T], caseType: CaseType):StringMatcher[T] = apply(words.keys, caseType, w => words(w))
  def apply[T](words: Iterable[String], caseType: CaseType, func: String => T):StringMatcher[T] = {
    Node.number = 1
    new StringMatcherImpl[T, T](words, caseType, func, (output, position) => output)
  }
  def contextMatcher(words: Iterable[String], caseType: CaseType):StringMatcher[MatchPosition] = {
    Node.number = 1
    new StringMatcherImpl[String, MatchPosition](words, caseType, identity[String],
    (output, position) => MatchPosition(output, position - output.length(), position))
  }
}



