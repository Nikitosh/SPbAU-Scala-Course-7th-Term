package com.nikitosh.spbau.parser

import com.nikitosh.spbau.lexer.Token

trait Parser {
  /** First token should be opening bracket matching with last closing bracket token. */
  def parse(tokens: List[Token]): Expression
}
