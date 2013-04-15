package net.szumo.fstl

import org.scalatest._

class BaseApi extends FlatSpec {

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

}