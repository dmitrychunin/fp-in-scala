package ru

import org.junit.Test
import org.scalatest.Assertions

class Chapter3Test extends Assertions {
  private val integerList = Cons(1, Cons(2, Cons(3, Nil)))
  private val doubleList = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))
  private val integerTree = Branch(
    Leaf(1),
    Branch(
      Branch(Leaf(3), Leaf(4)),
      Leaf(2)
    )
  )

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
    assert(integerList == Chapter3.drop(integerList, 0))
    assert(Nil == Chapter3.drop(integerList, 5))
  }

  //  EXERCISE 3.5
  @Test def shouldDropAllElementsIfTheyMatchPredicate(): Unit = {
    assert(Nil == Chapter3.dropWhile(integerList, (x: Int) => x > 0))
    assert(integerList == Chapter3.dropWhile(integerList, (x: Int) => x < 0))
    assert(Nil == Chapter3.dropWhile(Nil, (_: Int) => true))
  }

  //  EXERCISE 3.6
  @Test def shouldReturnLastElement(): Unit = {
    assert(Cons(1, Cons(2, Nil)) == Chapter3.init(integerList))
    assert(Nil == Chapter3.init(Nil))
    assert(Cons(1, Cons(2, Nil)) == Chapter3.init2(integerList))
    assert(Nil == Chapter3.init2(Nil))
  }

  //  EXERCISE 3.9
  @Test def shouldComputeListLength(): Unit = {
    assert(3 == Chapter3.length(integerList))
    assert(0 == Chapter3.length(Nil))
  }

  //  EXERCISE 3.11
  @Test def shouldSumAllListElements(): Unit = {
    assert(6 == Chapter3.sumLeft(integerList))
    assert(0 == Chapter3.sumLeft(Nil))
    assert(6 == Chapter3.productLeft(doubleList))
    assert(1 == Chapter3.productLeft(Nil))
    assert(3 == Chapter3.lengthLeft(integerList))
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

  //  EXERCISE 3.19 AND 3.21
  @Test def shouldFilterAllOddIntegersInList(): Unit = {
    assert(Cons(2, Nil) == Chapter3.filter(integerList)(i => i%2 == 0))
    val emptyIntegerList: List[Integer] = Nil
    assert(Nil == Chapter3.filter(emptyIntegerList)(i => i%2 == 0))

    assert(Cons(2, Nil) == Chapter3.filter2(integerList)(i => i % 2 == 0))
    val emptyIntegerList2: List[Integer] = Nil
    assert(Nil == Chapter3.filter2(emptyIntegerList2)(i => i % 2 == 0))
  }

  //  EXERCISE 3.20
  @Test def shouldDoubledListByFlatMap(): Unit = {
    assert(Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Nil)))))) == Chapter3.flatMap(integerList)(i => Cons(i, Cons(i, Nil))))
    assert(Nil == Chapter3.flatMap(Nil)(i => Cons(i, Cons(i, Nil))))
  }

  //  EXERCISE 3.22 AND 3.23
  @Test def shouldMergeTwoIntLists(): Unit = {
    assert(Cons(5, Cons(7, Cons(9, Nil))) == Chapter3.mergeTwoIntLists(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Cons(6, Nil)))).get)
    assert(Option.empty == Chapter3.mergeTwoIntLists(Cons(1, Nil), integerList))
    assert(Option.empty == Chapter3.mergeTwoIntLists(integerList, Cons(1, Nil)))

    assert(Cons(5, Cons(7, Cons(9, Nil))) == Chapter3.zipWith(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Cons(6, Nil))))(_ + _).get)
    assert(Option.empty == Chapter3.zipWith(Cons(1, Nil), integerList)(_ + _))
    assert(Option.empty == Chapter3.zipWith(integerList, Cons(1, Nil))(_ + _))
  }

  //  EXERCISE 3.24
  @Test def shouldFindSubsequences(): Unit = {
    assert(Chapter3.hasSubsequence(integerList, Cons(1, Cons(2, Nil))))
    assert(Chapter3.hasSubsequence(integerList, Cons(2, Cons(3, Nil))))
    assert(Chapter3.hasSubsequence(integerList, Cons(3, Nil)))
    assert(Chapter3.hasSubsequence(integerList, Nil))
    assert(!Chapter3.hasSubsequence(integerList, Cons(2, Cons(4, Nil))))
    assert(!Chapter3.hasSubsequence(integerList, Cons(4, Nil)))
  }

  //  EXERCISE 3.25
  @Test def shouldCountBranchesAndLeafs(): Unit = {
    assert(7 == Chapter3.size(integerTree))
  }

  //  EXERCISE 3.26
  @Test def shouldFindMaxElementInATree(): Unit = {
    assert(4 == Chapter3.findMax(integerTree))
  }

  //  EXERCISE 3.27
  @Test def shouldFindMaxDepth(): Unit = {
    assert(4 == Chapter3.depth(integerTree))
  }

  //  EXERCISE 3.28
  @Test def shouldUpAllIntElementsByOne(): Unit = {
    assert(Branch(
      Leaf(2),
      Branch(
        Branch(Leaf(4), Leaf(5)),
        Leaf(3)
      )
    ) == Chapter3.map(integerTree)(_ + 1))
  }

    //  EXERCISE 3.29
  @Test def shouldCountSizeMaxDepthAndExecuteMap(): Unit = {
    assert(7 == Chapter3.sizeRight(integerTree))
    assert(4 == Chapter3.findMaxRight(integerTree))
    assert(4 == Chapter3.depthRight(integerTree))
    assert(Branch(
      Leaf(2),
      Branch(
        Branch(Leaf(4), Leaf(5)),
        Leaf(3)
      )
    ) == Chapter3.mapRight(integerTree)(_ + 1)(Leaf(0)))
  }
}