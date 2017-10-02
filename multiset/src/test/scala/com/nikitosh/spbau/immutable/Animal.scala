package com.nikitosh.spbau.immutable

class Animal(val name: String) {
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case obj: Animal => name == obj.name
      case _           => false
    }
  }
}

class Cat(override val name: String) extends Animal(name)
