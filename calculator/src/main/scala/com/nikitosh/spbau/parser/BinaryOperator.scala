package com.nikitosh.spbau.parser

sealed trait BinaryOperatorType
case class Plus()     extends BinaryOperatorType
case class Minus()    extends BinaryOperatorType
case class Multiply() extends BinaryOperatorType
case class Divide()   extends BinaryOperatorType

class BinaryOperator(left: Expression, right: Expression, binaryOperatorType: BinaryOperatorType)
    extends Expression {
  override def evaluate(): Double = {
    binaryOperatorType match {
      case Plus()     => left.evaluate() + right.evaluate()
      case Minus()    => left.evaluate() - right.evaluate()
      case Multiply() => left.evaluate() * right.evaluate()
      case Divide()   => left.evaluate() / right.evaluate()
    }
  }
}
