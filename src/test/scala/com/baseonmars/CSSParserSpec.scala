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

  it should "identify an at-keyword" in {
    CSSParser.parse(CSSParser.ATKEYWORD, "@parp").get should equal(AtKeyword("parp"))
  }

  it should "identify a string" in {
    CSSParser.parse(CSSParser.STRING, "'astring'").get should equal(("'astring'"))
    CSSParser.parse(CSSParser.STRING, "\"the word\"").get should equal(("\"the word\""))
  }

  it should "identify a number" in {
    CSSParser.parse(CSSParser.NUMBER, "1").get should equal(Number("1"))
  }

  it should "identify a percentage" in {
    CSSParser.parse(CSSParser.PERCENTAGE,"23%").get should equal(Percentage(23.0))
  }

  it should "find a dimension" in {
    CSSParser.parse(CSSParser.DIMENSION, "10.4px").get should equal (Dimension(Number("10.4"), "px"))
  }

  it should "find a uri" in {
    CSSParser.parse(CSSParser.URI, "url(hello/world)").get should equal (Uri("url(hello/world)"))
  }

  it should "find a hash" in {
    CSSParser.parse(CSSParser.HASH, "#44d23").get should equal (Hash("#44d23"))
  }

  it should "find a unicode range" in {
    CSSParser.parse(CSSParser.UNICODE_RANGE, "\\2193").get should equal (Unicode("\u2193"))
  }

  it should "find an includes operator" in {
    CSSParser.parse(CSSParser.INCLUDES, "~=").get should equal(Includes())
  }

  it should "find a dash-match operator" in {
    CSSParser.parse(CSSParser.DASHMATCH, "|=").get should equal(DashMatch())
  }

  it should "match a function" in {
    CSSParser.parse(CSSParser.FUNCTION, "func1( a, b)").get should equal(Function("func1"))
  }

  "The any parser" should "find a number" in {
    CSSParser.parse(CSSParser.any, "5").get should equal(Number("5"))
  }

  it should "find a string" in {
    CSSParser.parse(CSSParser.any, "'string'").get should equal(("'string'"))
  }

  it should "find a percentage" in {
    CSSParser.parse(CSSParser.any, "24%").get should equal (Percentage(24.0))
  }

  it should "find a dimension" in {
    CSSParser.parse(CSSParser.any, "10.4px").get should equal (Dimension(Number("10.4"), "px"))
  }

  it should "find a uri" in {
    CSSParser.parse(CSSParser.any, "url(http://hello/world)").get should equal (Uri("url(http://hello/world)"))
  }

  it should "find a hash" in {
    CSSParser.parse(CSSParser.any, "#44d23").get should equal (Hash("#44d23"))
  }

  it should "find a unicode range" in {
    CSSParser.parse(CSSParser.any, "\\2193").get should equal (Unicode("\u2193"))
  }

  it should "find an includes operator" in {
    CSSParser.parse(CSSParser.any, "~=").get should equal(Includes())
  }

  it should "find a dash-match operator" in {
    CSSParser.parse(CSSParser.any, "|=").get should equal(DashMatch())
  }

  it should "parse this" in {

    val css = """.menuNumber1.menu {
      |  left: 100px;
      |  right: 100px;
      |  position: absolute;
      |  /* Optional Drop Down Styles, they only show when present */
      |
      |}
      |.menuNumber1.menu > .menuItem {
      |  padding: 0 0 10px 0;
      |}
      |.menuNumber1.menu.horizontal > .menuItem {
      |  padding: 0 10px 0 0;
      |}
      |.menuNumber1.menu .menuItem .menuItemDesign {
      |  background-color: #0c0c22;
      |  -webkit-border-radius: 5px 5px 5px 5px;
      |  -moz-border-radius: 5px 5px 5px 5px;
      |  border-radius: 5px 5px 5px 5px;
      |  -moz-background-clip: padding-box;
      |  -webkit-background-clip: padding-box;
      |  background-clip: padding-box;
      |  border-color: #eaeaea;
      |  border-color: rgba(234, 234, 234, 0.5);
      |  color: #eaeaea;
      |  color: rgba(234, 234, 234, 0.5);
      |  font-family: 'Times New Mars';
      |  font-size: 12px;
      |  font-style: normal;
      |  font-weight: 400;
      |  line-height: 15px;
      |  text-align: left;
      |  text-decoration: none;
      |  padding: 7px 7px 7px 7px;
      |}
      |.menuNumber1.menu .selected .menuItemDesign {
      |  background-color: #0c0c22;
      |  -webkit-border-radius: 5px 5px 5px 5px;
      |  -moz-border-radius: 5px 5px 5px 5px;
      |  border-radius: 5px 5px 5px 5px;
      |  -moz-background-clip: padding-box;
      |  -webkit-background-clip: padding-box;
      |  background-clip: padding-box;
      |  border-color: #eaeaea;
      |  border-color: rgba(234, 234, 234, 0.5);
      |  color: #eaeaea;
      |  color: rgba(234, 234, 234, 0.5);
      |  font-family: 'Times New Mars';
      |  font-size: 12px;
      |  font-style: normal;
      |  font-weight: 400;
      |  line-height: 15px;
      |  text-align: left;
      |  text-decoration: none;
      |  padding: 7px 7px 7px 7px;
      |}
      |.menuNumber1.menu .selected.menuItem:hover .menuItemDesign {
      |  background-color: #0c0c22;
      |  -webkit-border-radius: 5px 5px 5px 5px;
      |  -moz-border-radius: 5px 5px 5px 5px;
      |  border-radius: 5px 5px 5px 5px;
      |  -moz-background-clip: padding-box;
      |  -webkit-background-clip: padding-box;
      |  background-clip: padding-box;
      |  border-color: #eaeaea;
      |  border-color: rgba(234, 234, 234, 0.5);
      |  color: #eaeaea;
      |  color: rgba(234, 234, 234, 0.5);
      |  font-family: 'Times New Mars';
      |  font-size: 12px;
      |  font-style: normal;
      |  font-weight: 400;
      |  line-height: 15px;
      |  text-align: left;
      |  text-decoration: none;
      |  padding: 7px 7px 7px 7px;
      |}
      |.menuNumber1.menu .selected .subMenu .menuItemDesign {
      |  background: none;
      |  color: #eaeaea;
      |  color: rgba(234, 234, 234, 0.5);
      |  font-family: 'Times New Mars';
      |  font-size: 12px;
      |  font-style: normal;
      |  font-weight: 400;
      |  line-height: 15px;
      |  text-align: left;
      |  text-decoration: none;
      |  padding: 7px 7px 7px 7px;
      |}
      |.menuNumber1.menu .subMenu {
      |  padding: 0 0 0 5px;
      |}
      |.menuNumber1.menu.horizontal .subMenu {
      |  padding-top: 5px;
      |}
      |.menuNumber1.menu .subMenu ul {
      |  -webkit-border-radius: 5px 5px 5px 5px;
      |  -moz-border-radius: 5px 5px 5px 5px;
      |  border-radius: 5px 5px 5px 5px;
      |  -moz-background-clip: padding-box;
      |  -webkit-background-clip: padding-box;
      |  background-clip: padding-box;
      |  border-color: #eaeaea;
      |  border-color: rgba(234, 234, 234, 0.5);
      |  background-color: #0c0c22;
      |}
      |.menuNumber1.menu .subMenu .menuItem .menuItemDesign {
      |  color: #eaeaea;
      |  color: rgba(234, 234, 234, 0.5);
      |  font-family: 'Times New Mars';
      |  font-size: 12px;
      |  font-style: normal;
      |  font-weight: 400;
      |  line-height: 15px;
      |  text-align: left;
      |  text-decoration: none;
      |  padding: 7px 7px 7px 7px;
      |}
      |.menuNumber1.menu .subMenu .selected.menuItem .menuItemDesign {
      |  color: #eaeaea;
      |  color: rgba(234, 234, 234, 0.5);
      |  font-family: 'Times New Mars';
      |  font-size: 12px;
      |  font-style: normal;
      |  font-weight: 400;
      |  line-height: 15px;
      |  text-align: left;
      |  text-decoration: none;
      |  padding: 7px 7px 7px 7px;
      |}
      |.menuNumber1.menu .subMenu .menuItem:hover .menuItemDesign {
      |  color: #eaeaea;
      |  color: rgba(234, 234, 234, 0.5);
      |  font-family: 'Times New Mars';
      |  font-size: 12px;
      |  font-style: normal;
      |  font-weight: 400;
      |  line-height: 15px;
      |  text-align: left;
      |  text-decoration: none;
      |  padding: 7px 7px 7px 7px;
      |}
      |""".stripMargin

    CSSParser(css) should equal(StyleSheet(List()))
  }
}
