package com.nikitosh.spbau.lexer

trait Lexer {
  def splitIntoTokens(input: String): List[Token]
}
