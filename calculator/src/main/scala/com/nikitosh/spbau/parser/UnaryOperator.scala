package com.nikitosh.spbau.parser

sealed trait UnaryOperatorType
case class Sin()  extends UnaryOperatorType
case class Cos()  extends UnaryOperatorType
case class Log()  extends UnaryOperatorType
case class Sqrt() extends UnaryOperatorType

class UnaryOperator(argument: Expression, unaryOperatorType: UnaryOperatorType) extends Expression {
  override def evaluate(): Double = {
    unaryOperatorType match {
      case Sin()  => Math.sin(argument.evaluate())
      case Cos()  => Math.cos(argument.evaluate())
      case Log()  => Math.log(argument.evaluate())
      case Sqrt() => Math.sqrt(argument.evaluate())
    }
  }
}
