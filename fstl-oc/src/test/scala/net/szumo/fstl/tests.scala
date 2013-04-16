package net.szumo.fstl

import StringMatcher.{CaseSensitive, CaseInsensitive}
import org.scalatest._
import com.twitter.util.{Duration, Stopwatch}
import java.util.regex.Pattern

class BaseApi extends FlatSpec {
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

  "Edge cases" should "work with case insensitive also" in {
    val words = Seq("a", "ab", "bc", "bca", "C", "caa")
    val matcher = StringMatcher.contextMatcher(words, CaseInsensitive)
    assert(matcher("aBCcAB").toList === List(
      MatchPosition("a", 0, 1),
      MatchPosition("ab", 0, 2),
      MatchPosition("bc", 1, 3),
      MatchPosition("C", 2, 3),
      MatchPosition("C", 3, 4),
      MatchPosition("a", 4, 5),
      MatchPosition("ab", 4, 6)))
  }

  "Alternatives" should "work" in {
    val words = Seq("Szumo ma kota5", "Mary")
    val matcher = StringMatcher(words, CaseInsensitive)
    assert(matcher("Szumo ma kota5").toList === List("Szumo ma kota5"))
  }

}

class Timings extends FlatSpec with SequentialNestedSuiteExecution {
  def timeIt[T](message: String, f: => T):T = {
    val timer = Stopwatch.start()
    val result = f
    val elapsed = timer()
    println(s"Time for $message: $elapsed")
    result
  }
  val letters = "alam415fdskjgasbnvbm".toSet
  val words = (for (c1 <- letters; c2 <- letters; c3 <- letters) yield s"$c1$c2$c3").seq
  val string = "qwerypfm" * 100 + "ala ma kota"
  "Matchers" should "have timings" in {
    val matcher = timeIt("FSTL: Creating", StringMatcher(words, CaseInsensitive))
    timeIt("FSTL: Getting one match", matcher(string).next())
  }
  "Regex" should "have timings" in {
    val pattern = timeIt("Regex: Creating", Pattern.compile(words.mkString("|"), Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE))
    timeIt("Regex: Getting one match", pattern.matcher(string).find())
  }
  "Normal string matching" should "have timings" in {
    timeIt("Normal matching", {
      for (word <- words) {
        string.contains(word)
      }
    })
  }
  "RE2" should "have timings" in {
    import com.logentries.re2.{RE2, Options}
    val options = new Options().setMaxMem(128*1024*1024).setNeverCapture(true).setLogErrors(false).setCaseSensitive(false)
    val regex = timeIt("RE2: Creating", new RE2(words.mkString("|"), options))
    timeIt("RE2: Getting one match", regex.partialMatch(string))
    regex.close()
  }







}