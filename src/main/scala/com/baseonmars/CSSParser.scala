package com.baseonmars.cssparser

import util.parsing.combinator.RegexParsers
import sun.security.x509.CertificateSubjectUniqueIdentity


case class Selector(value: String)

case class AtKeyword(value: String)

abstract class CSSValue

package object implicits {


    implicit def cssString(s:String): CSSString = CSSString(s)


}
case class CSSString(value: String) extends CSSValue

case class Number(value:String) extends CSSValue

case class Percentage(value: Double) extends CSSValue

case class Dimension(value:Number, unit:String) extends CSSValue

case class Uri(value:String) extends CSSValue

case class Hash(value: String) extends CSSValue

case class Unicode(value: String) extends CSSValue

case class BadString(value: String)

case class Declaration(property: String, value: String)

case class RuleSet(selector: Selector, declarations: List[Declaration])

case class StyleSheet(rules: List[RuleSet])

case class Function(name:String)

case class Includes() {
  override def toString = "~="
}

case class DashMatch() {
  override def toString = "|="
}

object CSSParser extends RegexParsers {

  val nl = "\\n|\\r\\n|\\r|\\f"
  val nonascii = "[^\0-\237]"
  val unicode = """\\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?"""
  val escape = s"$unicode|\\[^\\n\\r\\f0-9a-f]"
  val nmstart = s"[_a-z]|$nonascii|$escape"
  val nmchar = s"[_a-z0-9-]|$nonascii|$escape"
  val name = s"($nmchar)+"
  val num = """([0-9]*\.[0-9]+|[0-9]+)"""
  val badcomment1 = """\/\*[^*]*\*+([^/*][^*]*\*+)*"""
  val badcomment2 = """\/\*[^*]*(\*+[^/*][^*]*)*""""
  val badcomment = s"$badcomment1|$badcomment2"
  val w = "[ \\t\\r\\n\\f]*"
  val ident = s"([-]?${nmstart}$nmchar)+"
  val string1 = s"""(\"([^\n\r\f\"]|\\$nl|$escape)*\")"""
  val string2	= s"\'([^\\n\\r\\f\\']|\\$nl|$escape)*\'"
  val string =  s"$string1|$string2"
  val uri = s"url\\(${w}${string}${w}\\)|url\\(${w}([!#"+"$"+s"%&*-\\[\\]-~]|${nonascii}|${escape})*${w}\\)"
  val hash = s"#${name}"
  val includes = "~="
  val dashmatch = "|="
  val function = s"${ident}\\("
  /**
   * Tokens
   */

  def IDENT:Parser[String] = opt("-") ~ nmstart.r ~ rep(nmchar.r) ^^ {
    case Some(pre) ~ start ~ rest => pre + start + rest.mkString
    case None ~ start ~ rest => start + rest.mkString
  }

  def ATKEYWORD:Parser[AtKeyword]= "@" ~> IDENT ^^ { case word => AtKeyword(word) }

  def STRING:Parser[String] = string.r ^^ { case s => s }

  def NUMBER:Parser[Number] = num.r ^^ { case n => Number(n) }

  def PERCENTAGE:Parser[Percentage] = NUMBER ~ "%" ^^ { case p ~ _ => Percentage(p.value.toDouble) }

  def DIMENSION:Parser[Dimension] = NUMBER ~ IDENT ^^ { case n ~ i => Dimension(n, i) }

  def URI: Parser[Uri] = uri.r ^^ { case u => Uri(u) }

  def HASH: Parser[Hash] = hash.r ^^ { case h => Hash(h) }

  def UNICODE_RANGE: Parser[Unicode] = unicode.r ^^ {
    case u =>
      Unicode(Character.toChars(Integer.parseInt(u.substring(1), 16)).mkString)
  }

  def INCLUDES:Parser[Includes] = includes.r ^^ { case i => Includes() }

  def DASHMATCH:Parser[DashMatch] =  dashmatch.r ^^ { case dm => DashMatch() }

  def FUNCTION:Parser[Function] = IDENT ~ "(" ~ repsep(any, ",") ~ ')' ^^ { case i ~ _ ~ _ ~ _=> Function(i) }

  def colon: Parser[String] = ":"

  def delim: Parser[String] = ";"

  def any = URI | UNICODE_RANGE | PERCENTAGE | DIMENSION |
    NUMBER | STRING | HASH | INCLUDES | IDENT | DASHMATCH | colon | delim

  def selector: Parser[String] = """(?i)[.\w]+""".r

  def property = s"(?i)$ident".r

  def value = s"(?i)($ident|$num)+".r    // TODO this should be any+

  /*
  [ IDENT | NUMBER | PERCENTAGE | DIMENSION | STRING
  | delim | URI | HASH | UNICODE-RANGE | INCLUDES
  | DASHMATCH | ':' | FUNCTION S* [any|unused]* ')'
  | '(' S* [any|unused]* ')' | '[' S* [any|unused]* ']'
  ] S*;
  */

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