package net.szumo.fstl

import scala.collection.GenTraversable

// StringMatcher is used to look for many words inside a string in one pass (using Aho-Corasick algorithm)

trait StringMatcher[T] {
  val words: GenTraversable[String]
  val caseSensitive:Boolean
  def isMatch(s: String) : Boolean
  def findMatches(s: String): Stream[T]
}

object StringMatcher {
  def apply(words: GenTraversable[String], caseSensitive:Boolean = true):StringMatcher[String] = {
    apply(words, identity, caseSensitive)
  }
  def apply[T](words: GenTraversable[String], func: String => T, caseSensitive:Boolean = true):StringMatcher[T] = {
    caseSensitive match {
      case true => new StringMatcherCaseSensitive[T](words, func)
      case false => new StringMatcherCaseInsensitive[T](words, func)
    }
  }
  def apppy[T](words: Map[String, T], caseSensitive:Boolean = true):StringMatcher[T] = {
    apply(words.keys, words.apply, caseSensitive)
  }
}
