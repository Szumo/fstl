package net.szumo.fstl

// StringMatcher is used to look for many words inside a string in one pass (using Aho-Corasick algorithm)
// StringMatcher maps words to outputs.

trait CaseType {
  def variants(c: Char): Iterable[Char]
}

trait StringMatcher[T] {
  def isMatch(s: String) : Boolean
  def isMatch(s: Iterable[String]) : Boolean
  def apply(s: String): Iterator[T]
  def apply(s: Iterable[String]): Iterator[T]
  def size: Int
}

object StringMatcher {
  object CaseSensitive extends CaseType {
    def variants(c: Char): Iterable[Char] = Seq(c)
  }
  object CaseInsensitive extends CaseType {
    def variants(c: Char): Iterable[Char] = if (c.isLower || c.isUpper) Seq(c.toUpper, c.toLower) else Seq(c)
  }

  def caseType(caseSensitive: Boolean):CaseType = if (caseSensitive) CaseSensitive else CaseInsensitive
  def apply(words: Iterable[String], caseType: CaseType):StringMatcher[String] = apply(words, caseType, identity _)
  def apppy[T](words: Map[String, T], caseType: CaseType):StringMatcher[T] = apply(words.keys, caseType, w => words(w))
  def apply[T](words: Iterable[String], caseType: CaseType, func: String => T):StringMatcher[T] = new StringMatcherImpl[T](words, caseType, func)
}



