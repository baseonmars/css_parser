package com.baseonmars.cssparser


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers


import com.baseonmars.cssparser.implicits._

class CSSParserSpec extends FlatSpec with ShouldMatchers {

  "A Css Parser" should "parse a ruleset" in {
    CSSParser(".hello { color: red }") should equal(StyleSheet(List(RuleSet(Selector(".hello"), List(Declaration("color", "red"))))))
  }

  it should "parse multiline rulesets" in {
    CSSParser(
      """.bob { color: red ;
        |font: Arial }""".stripMargin
    ) should equal(
      StyleSheet(List(RuleSet(Selector(".bob"), List(Declaration("color", "red"), Declaration("font", "Arial"))))))
  }

  it should "parse a mixed case ruleset" in {
    CSSParser( """.Thing { coloR: rEd }""") should equal(StyleSheet(List(RuleSet(Selector(".Thing"), List(Declaration("coloR", "rEd"))))))
  }

  it should "allow rulesets to end with a semicolon" in {
    CSSParser( """.sel { font-face: Arial; }""") should equal(StyleSheet(List(RuleSet(Selector(".sel"), List(Declaration("font-face", "Arial"))))))
  }

  it should "parse multiple rulesets" in {
    CSSParser(
      """.sel1 {
        |color: red;
        |}
        |.sel2 {
        |color: blue;
        |display: none;
        |width: 100px
        |}""".stripMargin
    ) should equal(StyleSheet(List(
      RuleSet(Selector(".sel1"), List(Declaration("color", "red"))),
      RuleSet(Selector(".sel2"), List(Declaration("color", "blue"), Declaration("display", "none"), Declaration("width", "100px")))
    )))
  }

}
