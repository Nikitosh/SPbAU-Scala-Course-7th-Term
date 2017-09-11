package com.nikitosh.spbau.parser

import com.nikitosh.spbau.lexer._

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

object ParserImpl extends Parser {
  val binaryOperators =
    HashMap("+" -> Plus(), "-" -> Minus(), "*" -> Multiply(), "/" -> Divide())
  val unaryOperators =
    HashMap("sin" -> Sin(), "cos" -> Cos(), "log" -> Log(), "sqrt" -> Sqrt())
  val binaryOperatorsPriorities =
    HashMap("+" -> 2, "-" -> 2, "*" -> 1, "/" -> 1)

  var operations: ArrayBuffer[Token] = ArrayBuffer[Token]()
  var expressions: ArrayBuffer[Expression] = ArrayBuffer[Expression]()

  override def parse(tokens: List[Token]): Expression = {
    //In the beginning of parsing situation is same as after binary operator.
    operations.clear()
    expressions.clear()
    var lastTokenClass: Class[_ <: Expression] = classOf[BinaryOperator]
    for (token <- tokens) {
      //print(token)
      //println(expressions)
      //println(operations)
      //println()
      token match {
        case Number(value) =>
          if (lastTokenClass == classOf[UnaryOperator] || lastTokenClass == classOf[Number]) {
            throw ParserException("Unexpected token found: " + value + ".")
          } else {
            expressions += new Number(value.toInt)
            lastTokenClass = classOf[Number]
          }

        case Operator(content) =>
          if (content == "-") {
            if (lastTokenClass == classOf[UnaryOperator]) {
              throw ParserException("Unexpected token found: '-'.")
            } else {
              if (lastTokenClass == classOf[BinaryOperator]) {
                //Change -x to 0-x for avoiding overcomplicated cases handling.
                expressions += new Number(0)
              }
              addBinaryOperator(Operator(content))
              lastTokenClass = classOf[BinaryOperator]
            }
          } else if (unaryOperators.contains(content)) {
            if (lastTokenClass == classOf[UnaryOperator] || lastTokenClass == classOf[Number]) {
              throw ParserException("Unexpected token found: " + content + ".")
            }
            addUnaryOperator(token)
            lastTokenClass = classOf[UnaryOperator]
          } else if (binaryOperators.contains(content)) {
            if (lastTokenClass == classOf[UnaryOperator] || lastTokenClass == classOf[BinaryOperator]) {
              throw ParserException("Unexpected token found: " + content + ".")
            }
            addBinaryOperator(Operator(content))
            lastTokenClass = classOf[BinaryOperator]
          } else {
            throw ParserException("Unknown token found: " + content + ".")
          }

        case OpeningBracket(_) =>
          if (lastTokenClass == classOf[Number]) {
            throw ParserException("Unexpected token found: '('.")
          } else {
            operations += token
            lastTokenClass = classOf[BinaryOperator]
          }

        case ClosingBracket(_) =>
          if (lastTokenClass == classOf[BinaryOperator]) {
            throw ParserException("Unexpected token found: ')'.")
          } else {
            while (operations.nonEmpty && operations.last.getClass != classOf[OpeningBracket]) {
              calculateLastOperator(operations.last.content)
            }
            if (operations.isEmpty) {
              throw ParserException("')' without matching '(' found.")
            }
            operations = operations.init
            lastTokenClass = classOf[Number]
            tryToApplyUnaryOperator()
          }
      }
    }
    if (operations.nonEmpty) {
      throw ParserException("Unmatched brackets found.")
    }
    assert(expressions.size == 1)
    expressions.last
  }

  def addBinaryOperator(operator: Operator): Unit = {
    while (operations.nonEmpty && operations.last.getClass == classOf[Operator]
           && binaryOperatorsPriorities(operator.content) >= binaryOperatorsPriorities(operations.last.content)) {
      calculateLastOperator(operations.last.content)
    }
    operations += operator
  }

  def addUnaryOperator(operation: Token): Unit = {
    operations += operation
  }

  def calculateLastOperator(operator: String): Unit = {
    val right = expressions.last
    expressions = expressions.init
    val left = expressions.last
    expressions = expressions.init
    operations = operations.init
    expressions += new BinaryOperator(left, right, binaryOperators(operator))
  }

  def tryToApplyUnaryOperator(): Unit = {
    if (operations.nonEmpty && operations.last.getClass == classOf[Operator]) {
      val content = operations.last.content
      if (unaryOperators.contains(content)) {
        val expression = expressions.last
        expressions = expressions.init
        operations = operations.init
        expressions += new UnaryOperator(expression, unaryOperators(content))
      }
    }
  }
}
