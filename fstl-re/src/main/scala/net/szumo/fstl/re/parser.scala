package net.szumo.fstl.re

/*
Simplified BNF for regular expression:

<RE> 	::= 	<union> | <simple-RE>
<union> 	::=	<RE> "|" <simple-RE>
<simple-RE> 	::= 	<concatenation> | <basic-RE>
<concatenation> 	::=	<simple-RE> <basic-RE>
<basic-RE> 	::=	<star> | <plus> | <elementary-RE>
<star> 	::=	<elementary-RE> "*"
<plus> 	::=	<elementary-RE> "+"
<elementary-RE> 	::=	<group> | <any> | <eos> | <char> | <set>
<group> 	::= 	"(" <RE> ")"
<any> 	::= 	"."
<eos> 	::= 	"$"
<char> 	::= 	any non metacharacter | "\" metacharacter
<set> 	::= 	<positive-set> | <negative-set>
<positive-set> 	::= 	"[" <set-items> "]"
<negative-set> 	::= 	"[^" <set-items> "]"
<set-items> 	::= 	<set-item> | <set-item> <set-items>
<set-items> 	::= 	<range> | <char>
<range> 	::= 	<char> "-" <char>
*/


sealed abstract class Part {
}
object Part {

}
case class CharRange(start: Char, stop: Char) extends Part
case class Alternative(options: Seq[Part]) extends Part
case class Repeat(lower: Option[Int], upper: Option[Int], what: Part) extends Part
case class Literal(char: Char) extends Part
case class Sequence(elements: Seq[Part]) extends Part
abstract class Metacharacter( extends Part
case object StartAnchor extends Part
case object EndAnchor extends Part

object FSTLRegexParser extends scala.util.parsing.combinator.RegexParsers {
  override type Elem = Char
  override def skipWhitespace = false

  def apply(input: String): Part = parseAll(RE, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => throw new Exception(failure.msg)
  }

  def RE = repsep(star | plus | simpleRE, "|") ^^ { case _ => EndAnchor }
  def simpleRE = elementaryRE.+ ^^ { case elements => Sequence(elements) }
  def star = elementaryRE.+ <~ "*" ^^ { case elements => Repeat(None, None, Sequence(elements)) }
  def plus = elementaryRE.+ <~ "+" ^^ { case elements => Repeat(Some(1), None, Sequence(elements)) }
  def elementaryRE = "." ^^ { case s => Literal(s.head) }
  def group = "(" ~> RE <~ ")"
  def stop = "$" ^^ { case _ => EndAnchor }
  def start = "^" ^^ { case _ => StartAnchor }
  def char = meta | nonmeta
  def meta = "\\" ~> ".".r ^^ { case x => Metacharacter(x) }


  /*
  def RE: P = { def apply(c:Char) = Literal(c) }

  def RE: P =            simpleRE ~ ('|' ~ simpleRE)
  def simpleRE: P =      star | plus | elementaryRE.*
  def star: P =          elementaryRE.* ~ "*"
  def plus: P =          elementaryRE.* ~ "+"
  def elementaryRE : P = group | any | eos | char | set
  def group : P =        "(" ~ RE ~ ")"
  def any : P =          "."
  def eos : P =          "$"
  def char : P =         ("\\" ~ meta) | nonmeta
  def set : P =          negativeset | positiveset
  def negativeset : P =  "[^" ~ setitem.+ ~ "]"
  def positiveset : P =  "[" ~ setitem.+ ~ "]"
  def setitem : P =      range | char
  def range : P =        nonmeta ~ "-" ~ nonmeta ^^ { case Literal(start) ~ "-" ~ Literal(stop) => Range(start, stop) }
  def nonmeta : P =      (c:Char) => Literal(c)
  def meta: P =          ".".r
  */

}