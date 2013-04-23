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

}

class Timings extends FlatSpec with SequentialNestedSuiteExecution {
  var times:Seq[String] = Seq.empty
  def timeIt[T](message: String, f: => T):T = {
    val timer = Stopwatch.start()
    val result = f
    val elapsed = timer().inMicroseconds
    times = times ++ Seq( s"Time for $message: $elapsed microseconds")
    result
  }
  val words = io.Source.fromInputStream(getClass.getResourceAsStream("/texts"),"utf-8").getLines().filter(s => s.nonEmpty).toIndexedSeq
  val word = words(700)
  val string = "qwerypfm" * 5 + word + "qwerypfm" * 5
  "Normal string matching" should "have timings" in {
    timeIt("Normal matching", {
      assert(Some(word) === words.find( s => string.contains(s) ))
    })
  }
  "Regex" should "have timings" in {
    val pattern = timeIt("Regex: Creating", Pattern.compile(words.mkString("|"), Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE))
    timeIt("Regex: Getting one match", assert(pattern.matcher(string).find() === true))
  }
  "RE2" should "have timings" in {
    import com.logentries.re2.{RE2, Options}
    val options = new Options().setMaxMem(128*1024*1024).setNeverCapture(true).setLogErrors(false).setCaseSensitive(false)
    val regex = timeIt("RE2: Creating", new RE2(words.mkString("|"), options))
    timeIt("RE2: Getting one match", assert(regex.partialMatch(string)===true))
    regex.close()
  }
  "Matchers" should "have timings" in {
    val matcher = timeIt("FSTL: Creating", StringMatcher(words, CaseInsensitive))
    timeIt("FSTL: Getting one match", assert(Some(word) === matcher(string).toStream.headOption))
  }
  "Times" should "show" in {
    println(times.mkString("\n"))
  }
}