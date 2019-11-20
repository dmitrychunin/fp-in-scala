package ru

import org.junit.Test
import org.scalatest.Assertions

class Chapter2Test extends Assertions {
  @Test def fibbonachiPositiveTest(): Unit = {
    assert(5 == Chapter2.fib(6))
  }

  @Test def fibbonachiBoundaryValueTest(): Unit = {
    assert(1 == Chapter2.fib(1))
    assert(1 == Chapter2.fib(2))
  }

  @Test def isSortedPositiveTest(): Unit = {
    val ascendingOrder: (Int, Int) => Boolean = (x: Int, y: Int) => x <= y
    assert(Chapter2.isSorted(Array(-1, 0, 6, 6, 8, 9), ascendingOrder))
  }

  @Test def isSortedNegativeTest(): Unit = {
    val ascendingOrder: (Int, Int) => Boolean = (x: Int, y: Int) => x <= y
    assert(!Chapter2.isSorted(Array(1, -4, 6, 0, 8, 9), ascendingOrder))
  }
}

