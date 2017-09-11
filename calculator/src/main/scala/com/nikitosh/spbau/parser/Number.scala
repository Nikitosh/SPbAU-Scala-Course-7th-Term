package com.nikitosh.spbau.parser

class Number(value: Double) extends Expression {
  override def evaluate(): Double = value
}
