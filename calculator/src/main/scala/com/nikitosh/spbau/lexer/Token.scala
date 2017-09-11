package com.nikitosh.spbau.lexer

sealed trait Token {
  val content: String
}
case class Number(content: String) extends Token
case class Operator(content: String) extends Token
case class OpeningBracket(content: String) extends Token
case class ClosingBracket(content: String) extends Token
