package net.szumo.fstl

// StringMatcher is used to look for many words inside a string in one pass (using Aho-Corasick algorithm)
// StringMatcher maps words to outputs.
// ContextStringMatcher maps them to match positions.

trait CaseType {
  parent =>
  def apply[V](from: Node[V], c: Char, to: Node[V])
  def andThen(other: CaseType):CaseType = new CaseType {
    def apply[V](from: Node[V], c: Char, to: Node[V]) {
      parent.apply(from, c, to)
      other.apply(from, c, to)
    }
  }
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
    def apply[V](from: Node[V], c: Char, to: Node[V]) { from.transitions = from.transitions.updated(c, to) }
  }
  object CaseInsensitive extends CaseType {
    def apply[V](from: Node[V], c: Char, to: Node[V]) {
      from.transitions = if (c.isLower || c.isUpper) from.transitions.updated(c.toUpper, to).updated(c.toLower, to) else from.transitions.updated(c, to)
    }
  }

  def caseType(caseSensitive: Boolean):CaseType = if (caseSensitive) CaseSensitive else CaseInsensitive
  def apply(words: Iterable[String], caseType: CaseType):StringMatcher[String] = apply(words, caseType, identity _)
  def apppy[T](words: Map[String, T], caseType: CaseType):StringMatcher[T] = apply(words.keys, caseType, w => words(w))
  def apply[T](words: Iterable[String], caseType: CaseType, func: String => T):StringMatcher[T] = new StringMatcherImpl[T](words, caseType, func)
}



