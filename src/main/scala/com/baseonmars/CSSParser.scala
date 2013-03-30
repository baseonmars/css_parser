package com.baseonmars.cssparser

import util.parsing.combinator.RegexParsers

case class Selector(value: String)

case class Declaration(property: String, value: String)

case class RuleSet(selector: Selector, declarations: List[Declaration])

case class StyleSheet(rules: List[RuleSet])

object CSSParser extends RegexParsers {

  val nl = "\n|\r\n|\r|\f"
  val nonascii = "[^\0-\237]"
  val unicode = """\\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?"""
  val escape = s"$unicode|\\[^\n\r\f0-9a-f]"
  val nmstart = s"[_a-z]|$nonascii|$escape"
  val nmchar = s"[_a-z0-9-]|$nonascii|$escape"
  val name = s"$nmchar+"
  val num = """[0-9]+|[0-9]*\.[0-9]+"""
  val badcomment1 = """\/\*[^*]*\*+([^/*][^*]*\*+)*"""
  val badcomment2 = """\/\*[^*]*(\*+[^/*][^*]*)*""""
  val badcomment = s"$badcomment1|$badcomment2"
  val w = "[ \t\r\n\f]*"
  val ident = s"([-]?${
    nmstart
  }$nmchar)*"

  def selector: Parser[String] = """(?i)\.\w+""".r

  def property: Parser[String] = s"(?i)$ident".r

  def value: Parser[String] = s"(?i)$ident".r

  def colon: Parser[String] = ":"

  def delim: Parser[String] = ";"

  def space = """[ \t\r\n\f]+"""

  def declaration: Parser[Declaration] = property ~ colon ~ value ^^ {
    case p ~ ":" ~ v => Declaration(p, v)
  }

  def ruleset: Parser[RuleSet] = selector ~ "{" ~ rep1sep(declaration, delim) ~ (delim ?) ~ "}" ^^ {
    case selector ~ "{" ~ declarations ~ _ ~ "}" => RuleSet(Selector(selector), declarations)
  }

  def stylesheet: Parser[StyleSheet] = rep1(ruleset) ^^ {
    case rules => StyleSheet(rules)
  }

  def apply(css: String) = parseAll(stylesheet, css) match {
    case Success(res, next) => res
    case fail => fail
  }
}