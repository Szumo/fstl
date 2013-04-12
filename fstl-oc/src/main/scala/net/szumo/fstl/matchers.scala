package net.szumo.fstl

import scala.collection.GenTraversable



protected abstract class StringMatcherBase[T](val words: GenTraversable[String], resultFunc: String => T)
  extends StringMatcher[T] {
  def isMatch(s: String) : Boolean = findMatches(s).isEmpty
  def findMatches(s: String): Stream[T] = Stream.empty




}

protected class StringMatcherCaseSensitive[T](words: GenTraversable[String], resultFunc: String => T) extends
  StringMatcherBase(words, resultFunc) {
  val caseSensitive = true
}

protected class StringMatcherCaseInsensitive[T](words: GenTraversable[String], resultFunc: String => T) extends
StringMatcherBase(words, resultFunc) {
  val caseSensitive = false
}

