package net.szumo.fstl.ac

// StringMatcher is used to look for many words inside a string in one pass (using Aho-Corasick algorithm)
// StringMatcher maps words to outputs.


trait StringMatcher[T] {
  def isMatch(s: String) : Boolean
  def isMatch(s: Iterable[String]) : Boolean
  def apply(s: String): Iterator[T]
  def apply(s: Iterable[String]): Iterator[T]
  def size: Int
}

object StringMatcher {
  import net.szumo.fstl.{CaseSensitive, CaseInsensitive, CaseType}

  def apply(words: Iterable[String]):StringMatcher[String] = apply(words, CaseSensitive, identity _)
  def apply(words: Iterable[String], caseType: CaseType):StringMatcher[String] = apply(words, caseType, identity _)
  def apppy[T](words: Map[String, T]):StringMatcher[T] = apply(words.keys, CaseSensitive, w => words(w))
  def apppy[T](words: Map[String, T], caseType: CaseType):StringMatcher[T] = apply(words.keys, caseType, w => words(w))
  def apply[T](words: Iterable[String], caseType: CaseType, func: String => T):StringMatcher[T] = new StringMatcherImpl[T](words, caseType, func)
}



