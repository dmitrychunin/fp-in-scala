package ru

import org.junit.Test
import org.scalatest.Assertions

class Chapter2Test extends Assertions {
  private val as: Array[Int] = Array(-1, 0, 6, 6, 8, 9)
  private val ascendingOrder: (Int, Int) => Boolean = (x: Int, y: Int) => x <= y
  private val descendingOrder: (Int, Int) => Boolean = (x: Int, y: Int) => x >= y

  //  EXERCISE 2.1
  @Test def shouldCalcFibbonachiNumber(): Unit = {
    assert(5 == Chapter2.fib(6))
    assert(0 == Chapter2.fib(1))
    assert(1 == Chapter2.fib(2))
  }

  //  EXERCISE 2.2
  @Test def shouldFindIsArraySorted(): Unit = {
    assert(Chapter2.isSorted(as, ascendingOrder))
    assert(!Chapter2.isSorted(as, descendingOrder))
  }

  //  EXERCISE 2.3
  @Test def curriedIsSortedPositiveAndNegativeTest(): Unit = {
    val partiallyAppliedIsSorted = Chapter2.curry((anyArray: Array[Int], anySortingPredicate: (Int, Int) => Boolean) => Chapter2.isSorted(anyArray, anySortingPredicate)).apply(as)
    assert(partiallyAppliedIsSorted.apply(ascendingOrder))
    assert(!partiallyAppliedIsSorted.apply(descendingOrder))
  }

  //  EXERCISE 2.4
  @Test def uncurriedCurriedIsSortedPositiveTest(): Unit = {
    val curriedIsSorted = Chapter2.curry((anyArray: Array[Int], anySortingPredicate: (Int, Int) => Boolean) => Chapter2.isSorted(anyArray, anySortingPredicate))
    assert(Chapter2.uncurry(curriedIsSorted).apply(as, ascendingOrder))
  }

  //  EXERCISE 2.5
  @Test def composePositiveTest(): Unit = {
    val doubleAbs = (a: Double) => Math.abs(a)
    val doubleSin = (a: Double) => Math.sin(a)
    val angle = 0.5
    assert(Math.abs(Math.sin(angle)) == Chapter2.compose(doubleAbs, doubleSin)(angle))
  }
}