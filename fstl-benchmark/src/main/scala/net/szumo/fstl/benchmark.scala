package net.szumo.fstl

import StringMatcher.CaseInsensitive
import com.twitter.util.Stopwatch
import java.util.regex.Pattern
import com.logentries.re2.{RE2, Options}

object Timings extends App {
  var times:Seq[String] = Seq.empty
  def timeIt[T](message: String, f: => T):T = {
    val timer = Stopwatch.start()
    val result = f
    val elapsed = timer().inMicroseconds
    times = times ++ Seq( s"Time for $message: $elapsed microseconds")
    result
  }

  def withWarmup(functions: Seq[() => Any]) {
    functions.foreach(f => for (i <- 1.to(5)) f())
    times = Seq.empty
    functions.foreach(f => f())
  }

  // FSTL
  def ahocorasick() {
    val words = io.Source.fromInputStream(getClass.getResourceAsStream("/texts"),"utf-8").getLines().filter(s => s.nonEmpty).toIndexedSeq
    val word = words(700)
    val string = "qwerypfm" * 5 + word + "qwerypfm" * 5
    timeIt("Normal matching", assert(Some(word) == words.find( s => string.contains(s) )))
    val pattern = timeIt("Regex: Creating", Pattern.compile(words.mkString("|"), Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE))
    timeIt("Regex: Getting one match", assert(pattern.matcher(string).find() == true))

    val options = new Options().setMaxMem(128*1024*1024).setNeverCapture(true).setLogErrors(false).setCaseSensitive(false)
    val regex = timeIt("RE2: Creating", new RE2(words.mkString("|"), options))
    timeIt("RE2: Getting one match", assert(regex.partialMatch(string)==true))
    regex.close()
    val matcher = timeIt("FSTL: Creating", StringMatcher(words, CaseInsensitive))
    timeIt("FSTL: Getting one match", assert(Some(word) == matcher(string).toStream.headOption))

  }

  withWarmup(Seq(ahocorasick _))
  println(times.mkString("\n"))

}