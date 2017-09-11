package com.nikitosh.spbau.lexer

import scala.collection.mutable.ArrayBuffer

object LexerImpl extends Lexer {
  override def splitIntoTokens(input: String): List[Token] = {
    val numberRegex         = """^(\d+)(.*)""".r
    val operatorRegex       = """^(\+|\-|\*|/|[^\d\+\-\*/\(\)]+)(.*)""".r
    val openingBracketRegex = """^(\()(.*)""".r
    val closingBracketRegex = """^(\))(.*)""".r
    val tokens = ArrayBuffer[Token]()
    var currentInput = input
    while (!currentInput.isEmpty) {
      currentInput = currentInput match {
        case numberRegex(content, restInput) =>
          tokens += Number(content)
          restInput
        case operatorRegex(content, restInput) =>
          tokens += Operator(content)
          restInput
        case openingBracketRegex(content, restInput) =>
          tokens += OpeningBracket(content)
          restInput
        case closingBracketRegex(content, restInput) =>
          tokens += ClosingBracket(content)
          restInput
      }
    }
    tokens.toList
  }
}
