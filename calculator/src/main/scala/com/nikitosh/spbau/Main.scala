package com.nikitosh.spbau

import com.nikitosh.spbau.lexer.LexerImpl
import com.nikitosh.spbau.parser.{ParserException, ParserImpl}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Expected exactly one argument: string with expression.")
    }
    //This trick is used for avoiding additional handling of tokens on outer level.
    val input = "(" + args(0) + ")"
    val lexer = LexerImpl
    val parser = ParserImpl
    try {
      val tokens = lexer.splitIntoTokens(input)
      val expression = parser.parse(tokens)
      System.out.println(expression.evaluate())
    } catch {
      case exception: ParserException => System.err.println(exception.message)
    }
  }
}
