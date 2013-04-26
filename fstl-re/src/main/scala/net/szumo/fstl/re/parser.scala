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


import scala.util.parsing.combinator.{RegexParsers}

protected sealed abstract class Part
protected case class CharRange(start: Char, stop: Char)
protected case class Alternative(options: Seq[Part])
protected case object StartAnchor extends Part
protected case object EndAnchor extends Part


protected object FSTLRegexParser extends RegexParsers {
  override def skipWhitespace = false

  def apply(input: String): Part = parseAll(RE, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => throw new Exception(failure.msg)
  }

  def RE: P = new Parser[Part] { def apply(c:Char) = Literal(c) }
  /*
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