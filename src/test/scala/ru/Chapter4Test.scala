package ru

import org.junit.Test
import org.scalatest.Assertions

class Chapter4Test extends Assertions {

    //  EXERCISE 4.1
  @Test def shouldReturnJustTailOfList(): Unit = {
    assert(Some(3) == Some(2).map(_+1))
    assert(None == None.map(_+1))
    assert(Some(3) == Some(2).flatMap(x => Some(x+1)))
    assert(Some(3) == Some(2).map(_+1))
    assert(Some(3) == Some(2).map(_+1))
    assert(Some(3) == Some(2).map(_+1))
  }
}
