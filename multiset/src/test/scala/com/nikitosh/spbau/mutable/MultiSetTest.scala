package com.nikitosh.spbau.mutable

import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.fail

class MultiSetTest {

  @Test
  def testAdd(): Unit = {
    val set = MultiSet(1, 2, 3)
    set.add(4)
    assertEquals(1, set.count(4))
    set.add(2)
    assertEquals(2, set.count(2))
  }

  @Test
  def testFilter(): Unit = {
    val set = MultiSet(1, 2, 3, 4, 2)
    assertEquals(MultiSet(1, 2, 2), set.filter(elem => elem <= 2))
  }

  @Test
  def testMap(): Unit = {
    val set = MultiSet(1, 2, 3, 4, 2)
    assertEquals(MultiSet(3, 4, 5, 6, 4), set.map(elem => elem + 2))
  }

  @Test
  def testFlatMap(): Unit = {
    val set = MultiSet(1, 2, 3, 1)
    assertEquals(MultiSet(1, 2, 2, 3, 3, 3, 1), set.flatMap(elem => List.fill(elem)(elem)))
  }

  @Test
  def testFind(): Unit = {
    val set = MultiSet(1, 1, 2, 3)
    assertEquals(Some(1), set.find(1))
    assertEquals(Some(2), set.find(2))
    assertEquals(None, set.find(4))
  }

  @Test
  def testIntersection(): Unit = {
    val set1 = MultiSet(1, 2, 2, 3, 3)
    val set2 = MultiSet(1, 3, 3, 3)
    assertEquals(MultiSet(1, 3, 3), set1 & set2)
  }

  @Test
  def testUnion(): Unit = {
    val set1 = MultiSet(1, 2, 2, 3, 3)
    val set2 = MultiSet(1, 3, 3, 3)
    assertEquals(MultiSet(1, 1, 2, 2, 3, 3, 3, 3, 3), set1 | set2)
  }

  @Test
  def testForComprehension(): Unit = {
    val set  = MultiSet(1, 2, 3, 1)
    var size = 0
    for (_ <- set) {
      size += 1
    }
    assertEquals(4, size)
  }

  @Test
  def testPatternMatching(): Unit = {
    val set = MultiSet(1, 2, 3, 1)
    set match {
      case MultiSet(e1, e2, e3, e4) => assertEquals(set, MultiSet(e1, e2, e3, e4))
      case _                        => fail()
    }
  }
}
