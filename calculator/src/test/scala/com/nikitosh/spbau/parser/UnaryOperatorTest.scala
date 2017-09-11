package com.nikitosh.spbau.parser

import org.junit.Assert.assertEquals
import org.junit.Test

class UnaryOperatorTest {
  val EPS = 1e-5

  @Test
  def testEvaluate(): Unit = {
    assertEquals(Math.sin(10), new UnaryOperator(new Number(10), Sin()).evaluate(), EPS)
    assertEquals(Math.cos(10), new UnaryOperator(new Number(10), Cos()).evaluate(), EPS)
    assertEquals(Math.log(10), new UnaryOperator(new Number(10), Log()).evaluate(), EPS)
    assertEquals(Math.sqrt(10), new UnaryOperator(new Number(10), Sqrt()).evaluate(), EPS)
  }
}
