package com.nikitosh.spbau.immutable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class MultiSet[+A](val elementsOccurrencesNumber: List[(A, Int)]) {

  def count[B >: A](elem: B): Int = {
    elementsOccurrencesNumber.filter(e => e._1 == elem).foldLeft(0)((sum, e) => sum + e._2)
  }

  def filter(f: (A) => Boolean): MultiSet[A] = {
    new MultiSet[A](elementsOccurrencesNumber.filter(e => f(e._1)))
  }

  def map[B](f: (A) => B): MultiSet[B] = {
    val mappedElementsOccurrencesNumber = mutable.Map[B, Int]().withDefaultValue(0)
    for ((elem, occurrencesNumber) <- elementsOccurrencesNumber) {
      mappedElementsOccurrencesNumber(f(elem)) += occurrencesNumber
    }
    new MultiSet[B](mappedElementsOccurrencesNumber.toList)
  }

  def flatMap[B](f: (A) => List[B]): MultiSet[B] = {
    val mappedElementsOccurrencesNumber = mutable.Map[B, Int]().withDefaultValue(0)
    for ((elem, occurrencesNumber) <- elementsOccurrencesNumber) {
      val mappedList = f(elem)
      for (elem <- mappedList) {
        mappedElementsOccurrencesNumber(elem) += occurrencesNumber
      }
    }
    new MultiSet[B](mappedElementsOccurrencesNumber.toList)
  }

  def apply[B >: A](elem: B): Boolean = {
    elementsOccurrencesNumber.exists(e => e._1 == elem)
  }

  def find[B >: A](elem: B): Option[B] = {
    elementsOccurrencesNumber.find(e => e._1 == elem).map(e => e._1)
  }

  def &[B >: A](set: MultiSet[B]): MultiSet[B] = {
    val intersectionElementsOccurrencesNumber = mutable.Map[B, Int]().withDefaultValue(0)
    for ((elem, occurrencesNumber) <- set.elementsOccurrencesNumber) {
      val currentCount = count(elem)
      if (currentCount > 0) {
        intersectionElementsOccurrencesNumber(elem) += Math.min(occurrencesNumber, currentCount)
      }
    }
    new MultiSet[B](intersectionElementsOccurrencesNumber.toList)
  }

  def |[B >: A](set: MultiSet[B]): MultiSet[B] = {
    val intersectionElementsOccurrencesNumber = mutable.Map[B, Int]().withDefaultValue(0)
    for ((elem, occurrencesNumber) <- elementsOccurrencesNumber) {
      intersectionElementsOccurrencesNumber(elem) += occurrencesNumber
    }
    for ((elem, occurrencesNumber) <- set.elementsOccurrencesNumber) {
      intersectionElementsOccurrencesNumber(elem) += occurrencesNumber
    }
    new MultiSet[B](intersectionElementsOccurrencesNumber.toList)
  }

  def foreach(f: (A) => Unit): Unit = {
    for ((elem, occurrencesNumber) <- elementsOccurrencesNumber) {
      Range(0, occurrencesNumber).foreach(_ => f(elem))
    }
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case obj: MultiSet[A] => elementsOccurrencesNumber.toSet == obj.elementsOccurrencesNumber.toSet
      case _                => false
    }
  }
}

object MultiSet {
  def apply[A](elems: A*): MultiSet[A] = {
    val elementsOccurrencesNumber = mutable.Map[A, Int]().withDefaultValue(0)
    for (elem <- elems) {
      elementsOccurrencesNumber(elem) += 1
    }
    new MultiSet[A](elementsOccurrencesNumber.toList)
  }

  def unapplySeq[A](set: MultiSet[A]): Option[Seq[A]] = {
    val elems = new ArrayBuffer[A]()
    for ((elem, occurrencesNumber) <- set.elementsOccurrencesNumber) {
      elems ++= List.fill(occurrencesNumber)(elem)
    }
    Some(elems)
  }
}
