package net.szumo.fstl

import org.scalatest._
import java.util.regex
import scala.collection.immutable.BitSet

class BaseApi extends FlatSpec {
  regex.Pattern
  BitSet
  /*
  "String iterator" should "convert to char iterator correctly" in {
    val strings = Seq("ala", " ", "ma", " ", "kota")
    val iterator = strings.flatMap( s => s.toIterator)
    assert(iterator.drop(4).take(2) === Seq('m', 'a'))
  }


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

  "Edge cases" should "work with both case sensitive and insensitive" in {
    val words = Seq("a", "ab", "bc", "bca", "c", "caa")
    for (matcher <- Seq(StringMatcher(words, CaseSensitive),StringMatcher(words, CaseInsensitive))) {
      assert(matcher("abccab").toList === List("a", "ab", "bc", "c", "c", "a", "ab"))
    }
  }


  "Alternatives" should "work" in {
    val words = Seq("Szumo ma kota5", "Mary")
    val matcher = StringMatcher(words, CaseInsensitive)
    assert(matcher("Szumo ma kota5").toList === List("Szumo ma kota5"))
  }
  */
}

