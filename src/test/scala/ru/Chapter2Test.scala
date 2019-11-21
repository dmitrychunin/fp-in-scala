package ru

import org.junit.Test
import org.scalatest.Assertions

class Chapter2Test extends Assertions {
  @Test def fibbonachiPositiveTest(): Unit = {
    assert(5 == Chapter2.fib(6))
  }

  @Test def fibbonachiBoundaryValueTest(): Unit = {
    assert(0 == Chapter2.fib(1))
    assert(1 == Chapter2.fib(2))
  }

  private val as: Array[Int] = Array(-1, 0, 6, 6, 8, 9)
  @Test def isSortedPositiveTest(): Unit = {
    val ascendingOrder: (Int, Int) => Boolean = (x: Int, y: Int) => x <= y
    assert(Chapter2.isSorted(as, ascendingOrder))
  }

  @Test def isSortedNegativeTest(): Unit = {
    val descendingOrder: (Int, Int) => Boolean = (x: Int, y: Int) => x >= y
    assert(!Chapter2.isSorted(as, descendingOrder))
  }

  @Test def curriedIsSortedPositiveAndNegativeTest(): Unit = {
    val ascendingOrder: (Int, Int) => Boolean = (x: Int, y: Int) => x <= y
    val descendingOrder: (Int, Int) => Boolean = (x: Int, y: Int) => x >= y
    val as: Array[Int] = Array(-1, 0, 6, 6, 8, 9)
    val partiallyAppliedIsSorted = Chapter2.curry((anyArray: Array[Int], anySortingPredicate: (Int, Int) => Boolean) => Chapter2.isSorted(anyArray, anySortingPredicate)).apply(as)
    assert(partiallyAppliedIsSorted.apply(ascendingOrder))
    assert(!partiallyAppliedIsSorted.apply(descendingOrder))
  }

  @Test def uncurriedCurriedIsSortedPositiveTest(): Unit = {
    val ascendingOrder: (Int, Int) => Boolean = (x: Int, y: Int) => x <= y
    val as: Array[Int] = Array(-1, 0, 6, 6, 8, 9)
    val curriedIsSorted = Chapter2.curry((anyArray: Array[Int], anySortingPredicate: (Int, Int) => Boolean) => Chapter2.isSorted(anyArray, anySortingPredicate))
    assert(Chapter2.uncurry(curriedIsSorted).apply(as, ascendingOrder))
  }

  @Test def composePositiveTest(): Unit = {
    val doubleAbs = (a: Double) => Math.abs(a)
    val doubleSin = (a: Double) => Math.sin(a)
    val angle = 0.5
    assert(Math.abs(Math.sin(angle)) == Chapter2.compose(doubleAbs, doubleSin)(angle))
  }
}

