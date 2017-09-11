package com.nikitosh.spbau.parser

import com.nikitosh.spbau.lexer._
import org.junit.Assert.assertEquals
import org.junit.Test

class ParserImplTest {
  val EPS            = 1e-5
  val parser: Parser = ParserImpl

  @Test
  def testParse_singleNumber(): Unit = {
    assertEquals(123, evaluate(List(Number("123"))), EPS)
  }

  @Test
  def testParse_operatorPriorities(): Unit = {
    assertEquals(
      10,
      evaluate(List(Number("2"), Operator("+"), Number("3"), Operator("*"), Number("5"), Operator("-"), Number("7"))),
      EPS)
  }

  @Test
  def testParse_unaryMinusWithoutBrackets(): Unit = {
    assertEquals(
      -1,
      evaluate(List(Number("2"), Operator("+"), OpeningBracket("("), Operator("-"), Number("3"), ClosingBracket(")"))),
      EPS)
  }

  @Test
  def testParse_unaryMinusWithBrackets(): Unit = {
    assertEquals(
      -5,
      evaluate(List(Operator("-"), OpeningBracket("("), Number("2"), Operator("+"), Number("3"), ClosingBracket(")"))),
      EPS)
  }

  @Test
  def testParse_unaryOperators(): Unit = {
    assertEquals(
      -Math.sin(Math.sqrt(2) * 3),
      evaluate(
        List(
          Operator("-"),
          Operator("sin"),
          OpeningBracket("("),
          Operator("sqrt"),
          OpeningBracket("("),
          Number("2"),
          ClosingBracket(")"),
          Operator("*"),
          Number("3"),
          ClosingBracket(")")
        )),
      EPS
    )
  }

  @Test
  def testParse_longExpression(): Unit = {
    assertEquals(
      5 + 2 * (-Math.sin(9f / (-4) - 2 - 1) + 19) - 7,
      evaluate(
        List(
          Number("5"),
          Operator("+"),
          Number("2"),
          Operator("*"),
          OpeningBracket("("),
          Operator("-"),
          Operator("sin"),
          OpeningBracket("("),
          Number("9"),
          Operator("/"),
          OpeningBracket("("),
          Operator("-"),
          Number("4"),
          ClosingBracket(")"),
          Operator("-"),
          Number("2"),
          Operator("-"),
          Number("1"),
          ClosingBracket(")"),
          Operator("+"),
          Number("19"),
          ClosingBracket(")"),
          Operator("-"),
          Number("7"),
        )),
      EPS
    )
  }

  @Test(expected = classOf[ParserException])
  def testParse_unknownToken(): Unit = {
    evaluate(List(Operator("aaa")))
  }

  @Test(expected = classOf[ParserException])
  def testParse_twoBinaryOperators(): Unit = {
    evaluate(List(Number("2"), Operator("+"), Operator("+"), Number("3")))
  }

  @Test(expected = classOf[ParserException])
  def testParse_unmatchedOpeningBracket(): Unit = {
    evaluate(List(OpeningBracket("("), OpeningBracket("("), Number("2"), ClosingBracket(")")))
  }

  @Test(expected = classOf[ParserException])
  def testParse_unmatchedClosingBracket(): Unit = {
    evaluate(List(OpeningBracket("("), Number("2"), ClosingBracket(")"), ClosingBracket(")")))
  }

  def evaluate(tokens: List[Token]): Double =
    parser.parse(List(OpeningBracket("(")) ++ tokens ++ List(ClosingBracket(")"))).evaluate()
}
