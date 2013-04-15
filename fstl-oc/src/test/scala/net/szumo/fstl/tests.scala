package net.szumo.fstl

import org.scalatest._

class BaseApi extends FlatSpec {
  "String iterator" should "convert to char iterator correctly" in {
    val strings = Seq("ala", " ", "ma", " ", "kota")
    val iterator = strings.flatMap( s => s.toIterator)
    assert(iterator.drop(4).take(2) === Seq('m', 'a'))
  }

  /*
  "Empty matcher" should "match nothing" in {
    val words = Seq.empty[String]
    val matcher = StringMatcher(words, CaseSensitive)
    assert(matcher("ala").isEmpty)
    assert(matcher.isMatch("ala") === false)
    assert(matcher(Seq("ala", "ma", "kota")).isEmpty)
    assert(matcher.isMatch(Seq("ala", "ma", "kota")) === false)
  }

  "Single word matcher" should "find that word" in {
    val words = Seq("ma")
    val matcher = StringMatcher(words, CaseSensitive)
    assert(matcher("ala").isEmpty)
    assert(matcher.isMatch("ala") === false)
    assert(matcher.isMatch("ala ma kota") === true)
    assert(matcher(Seq("ala", "ma", "kota")).toSeq === Seq("ma"))
    assert(matcher.isMatch(Seq("ala", "ma", "kota")) === true)
    assert(matcher.isMatch(Seq("ala", "nie", "posiada", "kota")) === false)
  }
  */
  "Edge cases" should "work with context" in {
    val words = Seq("a", "ab", "bc", "bca", "c", "caa")
    val matcher = StringMatcher.contextMatcher(words, CaseSensitive)
    assert(matcher("abccab").toList === List(
      MatchPosition("a", 0, 1),
      MatchPosition("ab", 0, 2),
      MatchPosition("bc", 1, 3),
      MatchPosition("c", 2, 3),
      MatchPosition("c", 3, 4),
      MatchPosition("a", 4, 5),
      MatchPosition("ab", 4, 6)))
  }
}

class Timings extends FlatSpec {





}