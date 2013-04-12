package net.szumo.fstl

// Aho-Corasick implementation

trait StringMatcher[T] {
  def isMatch(s: String) : Boolean
  def findMatches(s: String): Stream[T]
}

object StringMatcher {
  def apply(words: String, caseSensitive:Boolean = true):StringMatcher[String] = null
}
