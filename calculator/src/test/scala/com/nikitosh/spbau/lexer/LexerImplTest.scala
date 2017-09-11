package com.nikitosh.spbau.lexer

import org.junit.Test
import org.junit.Assert.assertEquals

class LexerImplTest {
  val lexer: Lexer = LexerImpl

  @Test
  def testSplitIntoTokens_singleNumber(): Unit = {
    assertEquals(List(Number("123")), lexer.splitIntoTokens("123"))
  }

  @Test
  def testSplitIntoTokens_bracketsAndOperators(): Unit = {
    assertEquals(List(OpeningBracket("("), Operator("sin"), ClosingBracket(")")), lexer.splitIntoTokens("(sin)"))
  }

  @Test
  def testSplitIntoTokens_longExpression(): Unit = {
    assertEquals(
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
        Operator("-"),
        Number("4"),
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
      ),
      lexer.splitIntoTokens("5+2*(-sin(9/-4-2-1)+19)-7")
    )
  }

  @Test
  def testSplitIntoTokens_wrongExpression(): Unit = {
    //No exceptions expected, because lexer just splits expression into tokens.
    lexer.splitIntoTokens("aaa(")
  }
}
