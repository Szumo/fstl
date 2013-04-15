package net.szumo.fstl

import scala.collection.GenTraversable

// StringMatcher is used to look for many words inside a string in one pass (using Aho-Corasick algorithm)
// StringMatcher maps words to outputs.
// ContextStringMatcher maps them to match positions.

sealed trait CaseType
object CaseSensitive extends CaseType
object CaseInsensitive extends CaseType

case class MatchPosition(word:String, start: Integer, stop:Integer)

trait StringMatcher[T] {
  val words: GenTraversable[String]
  val caseType:CaseType
  def isMatch(s: String) : Boolean
  def isMatch(s: Iterable[String]) : Boolean
  def apply(s: String): Stream[T]
  def apply(s: Iterable[String]): Stream[T]
}

object StringMatcher {
  def caseType(caseSensitive: Boolean):CaseType = if (caseSensitive) CaseSensitive else CaseInsensitive
  def apply(words: GenTraversable[String], caseType: CaseType):StringMatcher[String] = apply(words, caseType, identity _)
  def apppy[T](words: Map[String, T], caseType: CaseType):StringMatcher[T] = apply(words.keys, caseType, w => words(w))
  def apply[T](words: GenTraversable[String], caseType: CaseType, func: String => T):StringMatcher[T] = new StringMatcherImpl(words, caseType, func)
  def contextMatcher(words: GenTraversable[String], caseType: CaseType):StringMatcher[MatchPosition] = new ContextStringMatcherImpl(words, caseType)
}



