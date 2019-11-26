package ru

import org.junit.Test
import org.scalatest.Assertions

class Chapter3Test extends Assertions {
  private val integerList = Cons(1, Cons(2, Cons(3, Nil)))
  private val doubleList = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))

  //  EXERCISE 3.2
  @Test def shouldReturnJustTailOfList(): Unit = {
    assert(Cons(2, Cons(3, Nil)) == Chapter3.tail(integerList))
  }

  //  EXERCISE 3.3
  @Test def shouldUpdateExistingHeadOfList(): Unit = {
    assert(Cons(0, Cons(2, Cons(3, Nil))) == Chapter3.setHead(0, integerList))
  }

  //  EXERCISE 3.4
  @Test def shouldDropFirstNElements(): Unit = {
    assert(Cons(3, Nil) == Chapter3.drop(integerList, 2))
  }

  @Test def shouldNotDropAnyElementsIfNIsZero(): Unit = {
    assert(integerList == Chapter3.drop(integerList, 0))
  }

  @Test def shouldDropAllListIfNIsGreaterThenListSize(): Unit = {
    assert(Nil == Chapter3.drop(integerList, 5))
  }

  //  EXERCISE 3.5
  @Test def shouldDropAllElementsIfTheyMatchPredicate(): Unit = {
    assert(Nil == Chapter3.dropWhile(integerList, (x: Int) => x > 0))
  }

  @Test def shouldNotDropAnyElementIfNoOneMatchPredicate(): Unit = {
    assert(integerList == Chapter3.dropWhile(integerList, (x: Int) => x < 0))
  }

  @Test def shouldNotDropAnyAndReturnNilIfListIsEmpty(): Unit = {
    assert(Nil == Chapter3.dropWhile(Nil, (_: Int) => true))
  }

  //  EXERCISE 3.6
  @Test def shouldReturnLastElement(): Unit = {
    assert(Cons(1, Cons(2, Nil)) == Chapter3.init(integerList))
  }

  @Test def shouldReturnNilIfListIsEmpty(): Unit = {
    assert(Nil == Chapter3.init(Nil))
  }

  @Test def shouldReturnLastElement2(): Unit = {
    assert(Cons(1, Cons(2, Nil)) == Chapter3.init2(integerList))
  }

  @Test def shouldReturnNilIfListIsEmpty2(): Unit = {
    assert(Nil == Chapter3.init2(Nil))
  }

  //  EXERCISE 3.9
  @Test def shouldComputeListLength(): Unit = {
    assert(3 == Chapter3.length(integerList))
  }

  @Test def shouldReturnZeroLengthOnEmptyList(): Unit = {
    assert(0 == Chapter3.length(Nil))
  }

  //  EXERCISE 3.11
  @Test def shouldSumAllListElements(): Unit = {
    assert(6 == Chapter3.sumLeft(integerList))
  }

  @Test def shouldReturnZeroSumIfListIsEmpty(): Unit = {
    assert(0 == Chapter3.sumLeft(Nil))
  }

  @Test def shouldProductAllListElements(): Unit = {
    assert(6 == Chapter3.productLeft(doubleList))
  }

  @Test def shouldReturnOneIfListIsEmpty(): Unit = {
    assert(1 == Chapter3.productLeft(Nil))
  }

  @Test def shouldReturnListLength(): Unit = {
    assert(3 == Chapter3.lengthLeft(integerList))
  }

  @Test def shouldReturnZeroLengthIfListIsEmpty(): Unit = {
    assert(0 == Chapter3.lengthLeft(Nil))
  }

  //  EXERCISE 3.12
  @Test def shouldReturnReverted(): Unit = {
    assert(Cons(3, Cons(2, Cons(1, Nil))) == Chapter3.reverse(integerList))
    assert(Nil == Chapter3.reverse(Nil))
  }

  //  EXERCISE 3.14
  @Test def shouldAppendOnEmptyAndNonEmptyList(): Unit = {
    assert(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))) == Chapter3.appendRight(integerList, 4))
    assert(Cons(4, Nil) == Chapter3.appendRight(Nil, 4))
    assert(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))) == Chapter3.appendRight2(integerList, 4))
    assert(Cons(4, Nil) == Chapter3.appendRight2(Nil, 4))
  }

  //  EXERCISE 3.16
  @Test def shouldTransformIntegerList(): Unit = {
    assert(Cons(2, Cons(3, Cons(4, Nil))) == Chapter3.listTransformer(integerList, 1)(_ + _))
    assert(Nil == Chapter3.listTransformer(Nil, 1)(_ + _))
  }

  //  EXERCISE 3.17 AND 3.18
  @Test def shouldTransformDoubleListIntoStringList(): Unit = {
    assert(Cons("1.0", Cons("2.0", Cons("3.0", Nil))) == Chapter3.doubleListToStringList(doubleList))
    assert(Nil == Chapter3.doubleListToStringList(Nil))
    assert(Cons("1.0", Cons("2.0", Cons("3.0", Nil))) == Chapter3.map(doubleList)(d => d.toString))
    assert(Nil == Chapter3.map(Nil)(d => d.toString))
  }

    //  EXERCISE 3.19
  @Test def shouldFilterAllOddIntegersInList(): Unit = {
    assert(Cons(2, Nil) == Chapter3.filter(integerList)(i => i%2 == 0))
    val emptyIntegerList: List[Integer] = Nil
    assert(Nil == Chapter3.filter(emptyIntegerList)(i => i%2 == 0))
  }
}
