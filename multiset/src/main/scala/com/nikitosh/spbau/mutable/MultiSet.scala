package com.nikitosh.spbau.mutable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class MultiSet[A](val elementsOccurrencesNumber: mutable.Map[A, Int] = mutable.Map[A, Int]().withDefaultValue(0)) {

  def add(elem: A, count: Int = 1): Unit = {
    elementsOccurrencesNumber(elem) += count
  }

  def count(elem: A): Int = {
    if (elementsOccurrencesNumber.contains(elem)) elementsOccurrencesNumber(elem) else 0
  }

  def filter(f: (A) => Boolean): MultiSet[A] = {
    val filteredSet = new MultiSet[A]()
    for ((elem, occurrencesNumber) <- elementsOccurrencesNumber) {
      if (f(elem)) {
        filteredSet.add(elem, occurrencesNumber)
      }
    }
    filteredSet
  }

  def map[B](f: (A) => B): MultiSet[B] = {
    val mappedSet = new MultiSet[B]()
    for ((elem, occurrencesNumber) <- elementsOccurrencesNumber) {
      mappedSet.add(f(elem), occurrencesNumber)
    }
    mappedSet
  }

  def flatMap[B](f: (A) => List[B]): MultiSet[B] = {
    val mappedSet = new MultiSet[B]()
    for ((elem, occurrencesNumber) <- elementsOccurrencesNumber) {
      val mappedList = f(elem)
      for (elem <- mappedList) {
        mappedSet.add(elem, occurrencesNumber)
      }
    }
    mappedSet
  }

  def apply(elem: A): Boolean = {
    elementsOccurrencesNumber.contains(elem)
  }

  def find(elem: A): Option[A] = {
    if (elementsOccurrencesNumber.contains(elem)) Some(elem) else None
  }

  def &(set: MultiSet[A]): MultiSet[A] = {
    val intersectionSet = new MultiSet[A]()
    for ((elem, occurrencesNumber) <- elementsOccurrencesNumber) {
      if (set.elementsOccurrencesNumber.contains(elem)) {
        intersectionSet.add(elem, Math.min(occurrencesNumber, set.elementsOccurrencesNumber(elem)))
      }
    }
    intersectionSet
  }

  def |(set: MultiSet[A]): MultiSet[A] = {
    val unionSet = new MultiSet[A]()
    for ((elem, occurrencesNumber) <- elementsOccurrencesNumber) {
      unionSet.add(elem, occurrencesNumber)
    }
    for ((elem, occurrencesNumber) <- set.elementsOccurrencesNumber) {
      unionSet.add(elem, occurrencesNumber)
    }
    unionSet
  }

  def foreach(f: (A) => Unit): Unit = {
    for ((elem, occurrencesNumber) <- elementsOccurrencesNumber) {
      Range(0, occurrencesNumber).foreach(_ => f(elem))
    }
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case obj: MultiSet[A] => elementsOccurrencesNumber == obj.elementsOccurrencesNumber
      case _                => false
    }
  }
}

object MultiSet {
  def apply[A](elems: A*): MultiSet[A] = {
    val set = new MultiSet[A]()
    for (elem <- elems) {
      set.add(elem)
    }
    set
  }

  def unapplySeq[A](set: MultiSet[A]): Option[Seq[A]] = {
    val elems = new ArrayBuffer[A]()
    for ((elem, occurrencesNumber) <- set.elementsOccurrencesNumber) {
      elems ++= List.fill(occurrencesNumber)(elem)
    }
    Some(elems)
  }
}
