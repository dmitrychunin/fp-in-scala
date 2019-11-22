package ru

import org.junit.Test
import org.scalatest.Assertions

class Chapter3Test extends Assertions {
  private val list = Cons(1, Cons(2, Cons(3, Nil)))

  //  EXERCISE 3.2
  @Test def shouldReturnJustTailOfList(): Unit = {
    assert(Cons(2, Cons(3, Nil)) == Chapter3.tail(list))
  }

  //  EXERCISE 3.3
  @Test def shouldUpdateExistingHeadOfList(): Unit = {
    assert(Cons(0, Cons(2, Cons(3, Nil))) == Chapter3.setHead(0, list))
  }

  //  EXERCISE 3.4
  @Test def shouldDropFirstNElements(): Unit = {
    assert(Cons(3, Nil) == Chapter3.drop(list, 2))
  }

  @Test def shouldNotDropAnyElementsIfNIsZero(): Unit = {
    assert(list == Chapter3.drop(list, 0))
  }

  @Test def shouldDropAllListIfNIsGreaterThenListSize(): Unit = {
    assert(Nil == Chapter3.drop(list, 5))
  }

  //  EXERCISE 3.5
  @Test def shouldDropAllElementsIfTheyMatchPredicate(): Unit = {
    assert(Nil == Chapter3.dropWhile(list, (x: Int) => x > 0))
  }

  @Test def shouldNotDropAnyElementIfNoOneMatchPredicate(): Unit = {
    assert(list == Chapter3.dropWhile(list, (x: Int) => x < 0))
  }

  @Test def shouldNotDropAnyAndReturnNilIfListIsEmpty(): Unit = {
    assert(Nil == Chapter3.dropWhile(Nil, (_: Int) => true))
  }

  //  EXERCISE 3.6
  @Test def shouldReturnLastElement(): Unit = {
    assert(Cons(1, Cons(2, Nil)) == Chapter3.init(list))
  }

  @Test def shouldReturnNilIfListIsEmpty(): Unit = {
    assert(Nil == Chapter3.init(Nil))
  }
}
