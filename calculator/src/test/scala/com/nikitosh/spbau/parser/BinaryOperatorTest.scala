package com.nikitosh.spbau.parser

import org.junit.Test
import org.junit.Assert.assertEquals

class BinaryOperatorTest {
  val EPS = 1e-5

  @Test
  def testEvaluate(): Unit = {
    assertEquals(3, new BinaryOperator(new Number(1), new Number(2), Plus()).evaluate(), EPS)
    assertEquals(-1, new BinaryOperator(new Number(1), new Number(2), Minus()).evaluate(), EPS)
    assertEquals(2, new BinaryOperator(new Number(1), new Number(2), Multiply()).evaluate(), EPS)
    assertEquals(0.5, new BinaryOperator(new Number(1), new Number(2), Divide()).evaluate(), EPS)
  }
}
