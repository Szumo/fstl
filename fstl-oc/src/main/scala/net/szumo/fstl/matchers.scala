package net.szumo.fstl

import scala.collection.GenTraversable

protected abstract class StringMatcherBase[O,T](val words: GenTraversable[String], val caseType: CaseType, resultFunc: String => O) extends StringMatcher[T] {
  matcher =>
  def createResult(output:O, start:Integer, stop:Integer):T
  def isMatch(s: String) = !(new State(Iterator(s)).isEmpty)
  def isMatch(s: Iterable[String]) = !(new State(s.iterator).isEmpty)
  def apply(s: String):Stream[T] = new State(Iterator(s)).toStream
  def apply(s: Iterable[String]): Stream[T] = new State(s.iterator).toStream

  class State(strings: Iterator[String]) extends Iterator[T] {
    var position = 0
    def hasNext: Boolean = false
    def next(): T = throw new java.util.NoSuchElementException()
  }
}

protected class StringMatcherImpl[T](words: GenTraversable[String], caseType: CaseType, resultFunc: String => T) extends StringMatcherBase[T,T](words, caseType, resultFunc) {
  def createResult(output:T, start:Integer, stop:Integer):T = output
}

protected class ContextStringMatcherImpl(words: GenTraversable[String], caseType: CaseType) extends StringMatcherBase[String,MatchPosition](words, caseType, identity) {
  def createResult(output:String, start:Integer, stop:Integer):MatchPosition = MatchPosition(output, start, stop)
}

