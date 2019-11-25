package ru

//list
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

//tree
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Chapter3 {

  /*

  EXERCISE 3.2
  Implement the function tail for removing the first element of a List. Note that the
  function takes constant time. What are different choices you could make in your
  implementation if the List is Nil? We’ll return to this question in the next chapter.
   */
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  /*
  EXERCISE 3.3
  Using the same idea, implement the function setHead for replacing the first element
  of a List with a different value.
   */
  def setHead[A](newHead: A, list: List[A]): List[A] = list match {
    case Nil => Cons(newHead, Nil)
    case Cons(_, tail) => Cons(newHead, tail)
  }

  /*
  EXERCISE 3.4
  Generalize tail to the function drop, which removes the first n elements from a list.
  Note that this function takes time proportional only to the number of elements being
  dropped—we don’t need to make a copy of the entire List.
  def drop[A](l: List[A], n: Int): List[A]
   */

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop[A](l: List[A], i: Int): List[A] = l match {
      case Nil => Nil
      case Cons(_, tail) =>
        if (i > 0) loop(tail, i - 1)
        else l
    }

    loop(l, n)
  }

  /*
  EXERCISE 3.5
  Implement dropWhile, which removes elements from the List prefix as long as they
  match a predicate.
  def dropWhile[A](l: List[A], f: A => Boolean): List[A]
   */

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) =>
      if (f.apply(head)) dropWhile(tail, f)
      else l
  }

  /*
  EXERCISE 3.6
  Not everything works out so nicely. Implement a function, init, that returns a List
  consisting of all but the last element of a List. So, given List(1,2,3,4), init will
  return List(1,2,3). Why can’t this function be implemented in constant time like
  tail?
  def init[A](l: List[A]): List[A]
   */
  def init[A](list: List[A]): List[A] = {
    def loop(l: List[A], p: A): List[A] = l match {
      case Cons(_, Nil) => Cons(p, Nil)
      case Cons(h, t) => Cons(p, loop(t, h))
    }

    list match {
      case Nil => Nil
      case Cons(head, tail) => loop(tail, head)
    }
  }

  def init2[A](list: List[A]): List[A] = {

    @annotation.tailrec
    def loop(l: List[A], reverted: List[A]): List[A] = l match {
      case Cons(_, Nil) => reverted
      case Cons(h, t) => loop(t, Cons(h, reverted))
    }

    @annotation.tailrec
    def revert(reverted: List[A], result: List[A]): List[A] = reverted match {
      case Nil => result
      case Cons(h, t) => revert(t, Cons(h, result))
    }

    list match {
      case Nil => Nil
      case Cons(head, tail) => revert(loop(tail, Cons(head, Nil)), Nil)
    }
  }

  /*
  EXERCISE 3.7
  Can product, implemented using foldRight, immediately halt the recursion and
  return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
  might work if you call foldRight with a large list. This is a deeper question that we’ll
  return to in chapter 5.
   */
  /*
  EXERCISE 3.8
  See what happens when you pass Nil and Cons themselves to foldRight, like this:
  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)). What do you think this
  says about the relationship between foldRight and the data constructors of List?
   */
  /*
  EXERCISE 3.9
  Compute the length of a list using foldRight.
  def length[A](as: List[A]): Int
   */

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => y + 1)
  }

  /*
  EXERCISE 3.10
  Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError
  for large lists (we say it’s not stack-safe). Convince yourself that this is the
  case, and then write another general list-recursion function, foldLeft, that is
  tail-recursive, using the techniques we discussed in the previous chapter. Here is its
  signature:
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B
   */
  //todo if this implementation is right why this foldLeft (it's foldRight but tail-recoursive)?
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f.apply(z, head))(f)
  }

  /*
  EXERCISE 3.11
  Write sum, product, and a function to compute the length of a list using foldLeft.
   */
  //  todo what sense to duplicate sum, product, length from book if api for foldRight ~ foldLeft
  //  maybe i make foldLeft wrong and f() must be something different from foldRight's one?
  def sumLeft(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def productLeft(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def lengthLeft[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => y + 1)
  }

  /*
  EXERCISE 3.12
  Write a function that returns the reverse of a list (given List(1,2,3) it returns
  List(3,2,1)). See if you can write it using a fold.
   */
  //  def reverse[A](as: List[A]): List[A] = as match {
  //        case Nil => Nil
  //    case Cons(_, Nil) => as
  //    case Cons(head, tail) => loop(tail, Cons(head, Nil))
  //
  //      @annotation.tailrec
  //      def loop(as: List[A], reversed: List[A]): List[A] = as match {
  //        case Nil => reversed
  //        case Cons(head, tail) => loop(tail, Cons(head, reversed))
  //      }
  //      todo why returned type is Unit?
  //  }

  /*
  EXERCISE 3.13
  Hard: Can you write foldLeft in terms of foldRight? How about the other way
  around? Implementing foldRight via foldLeft is useful because it lets us implement
  foldRight tail-recursively, which means it works even for large lists without overflowing
  the stack.
   */
  //  def tailRecursiveFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
  //    todo if i change
  //   foldLeft(as,z)(f)
  //  }
  /*
  EXERCISE 3.14
  Implement append in terms of either foldLeft or foldRight.
   */
  /*
  EXERCISE 3.15
  Hard: Write a function that concatenates a list of lists into a single list. Its runtime
  should be linear in the total length of all lists. Try to use functions we have already
  defined.
   */
  /*
  EXERCISE 3.16
  Write a function that transforms a list of integers by adding 1 to each element.
  (Reminder: this should be a pure function that returns a new List!)
   */
  //  todo make tailrec
  //  @annotation.tailrec
  //  todo use more abstract map instead
  def listTransformer[A, B](as: List[A], op: B)(f: (A, B) => A): List[A] = as match {
    case Cons(head, Nil) => Cons(f.apply(head, op), Nil)
    case Cons(head, tail) => Cons(f.apply(head, op), listTransformer(tail, op)(f))
  }

  /*
  EXERCISE 3.17
  Write a function that turns each value in a List[Double] into a String. You can use
  the expression d.toString to convert some d: Double to a String.
  /*
  EXERCISE 3.18
    Write a function map that generalizes modifying each element in a list
    while maintaining
    the structure of the list.Here is its signature:
    def map[A, B](as: List[A])(f: A => B): List[B]
  */
   */
  //  todo test map on Double to String converter example
  //  todo make tailrec
  //  @annotation.tailrec
  def map[A, B](s: List[A])(f: A => B): List[B] = s match {
    case Nil => Nil
    case Cons(head, Nil) => Cons(f.apply(head), Nil)
    case Cons(head, tail) => Cons(f.apply(head), map(tail)(f))
  }

  /*
  EXERCISE 3.19
    Write a function filter that removes elements from a list unless they satisfy a given
    predicate.Use it to remove all odd numbers from a List
    [Int].

    def filter[A](as: List[A])(f: A => Boolean): List[A]
  */

  //  todo tailrec
  //  @annotation.tailrec
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) => {
      val filteredTail = filter(tail)(f)
      if (f.apply(head)) Cons(head, filteredTail)
      else filteredTail
    }
  }

  /*
  EXERCISE 3.20
    Write a function flatMap that works like map except that the function given will
    return
    a list instead of a single result
    , and that list should be inserted into the
    final resulting
    list.Here is its signature:

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B]

    For instance, flatMap(List(1, 2, 3))(i => List(i, i)) should result in List(1, 1, 2, 2, 3, 3).
   */
  //  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
  //    case Nil => Nil
  //    case Cons(head, Nil) => f.apply(head)
  //    case Cons(head, tail) => merge(f.apply(head), flatMap(tail)(f))
  //  }
  //
  //  def merge[A, B](f: List[B], s: List[B]): List[B] = s match {
  //    case Nil => Nil
  //    case Cons(head, tail) => tail
  //
  //      def loop(as: List[B]): List[B] = as match {
  //        case Cons(head, tail) => loop(tail)
  //      }
  //  }

  /*
  EXERCISE 3.21
  Use flatMap to implement filter.
  */
  //  todo
  /*
  EXERCISE 3.22
  Write a function that accepts two lists and constructs a new list by adding corresponding
  elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
  */
  def mergeTwoIntLists(as: List[Int], bs: List[Int]): List[Int] = {
    //    todo check list size are the same?
    @annotation.tailrec
    def loop(as: List[Int], bs: List[Int], revertedResult: List[Int]): List[Int] = as match {
      case Nil => revertList(revertedResult, Nil)
      case Cons(headA, tailA) => bs match {
        case Cons(headB, tailB) => loop(tailA, tailB, Cons(headA + headB, revertedResult))
      }
    }

    loop(as, bs, Nil)
  }

  @annotation.tailrec
  def revertList[A](as: List[A], result: List[A]): List[A] = as match {
    case Nil => result
    case Cons(head, tail) => revertList(tail, Cons(head, result))
  }

  /*
  EXERCISE 3.23
  Generalize the function you just wrote so that it’s not specific to integers or addition.
  Name your generalized function zipWith.
   */

  def zipWith[A](as: List[A], bs: List[A], zip: (A, A) => A): List[A] = {
    @annotation.tailrec
    def loop(as: List[A], bs: List[A], zip: (A, A) => A, revertedResult: List[A]): List[A] = as match {
      case Nil => revertList(revertedResult, Nil)
      case Cons(headA, tailA) => bs match {
        case Cons(headB, tailB) => loop(tailA, tailB, zip, Cons(zip.apply(headA, headB), revertedResult))
      }
    }

    loop(as, bs, zip, Nil)
  }

  /*
  EXERCISE 3.24
  Hard: As an example, implement hasSubsequence for checking whether a List contains
  another List as a subsequence. For instance, List(1,2,3,4) would have
  List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
  some difficulty finding a concise purely functional implementation that is also efficient.
  That’s okay. Implement the function however comes most naturally. We’ll
  return to this implementation in chapter 5 and hopefully improve on it. Note: Any
  two values x and y can be compared for equality in Scala using the expression x == y.
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean
  */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def loop(currentSup: List[A], originSub: List[A], currentSub: List[A]): Boolean = currentSub match {
      //          sub был либо изначально Nil либо стал таким после рекурсивного изменения
      case Nil => true
      case Cons(headSub, tailSub) => currentSup match {
        //          sup закончился а sub так полностью и не был рекурсивно пройден
        case Nil => true
        case Cons(headSup, tailSup) => {
          //          если найдено совпадение то уменьшаем текущий sup и sub
          if (headSup == headSub) loop(tailSup, sub, tailSub)
          //            если совпадения нет, то уменьшаем только sup, currentSub сбрасываем в начальное состояние
          else loop(tailSup, sub, sub)
        }
      }
    }

    loop(sup, sub, sub)
  }


  /*
  EXERCISE 3.25
  Write a function size that counts the number of nodes (leaves and branches) in a tree.
  */
  def size[A](t: Tree[A]): Int = {
    //    @annotation.tailrec
    def loop(tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => loop(left) + loop(right) + 1
    }

    loop(t)
  }

  /*
  EXERCISE 3.26
  Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
  In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
  and y.)
  */
  /*
  EXERCISE 3.27
  Write a function depth that returns the maximum path length from the root of a tree
  to any leaf.
  */
  //  def depth[A](tree: Tree[A]): Int = {
  //    def loop(tree: Tree[A], currentLevel: Int): Int = tree match {
  //      case Leaf(value) => currentLevel
  //      case Branch(left, right) => {
  //        val leftDepth = loop(left, currentLevel + 1)
  //        val rightDepth = loop(right, currentLevel + 1)
  //        max(leftDepth, rightDepth)
  //        leftDepth.max(ri)
  //      }
  //    }
  //
  //    loop(tree, 0)
  //  }
  //
  //  def max[A](f: A, s: A): A = {
  //    if (f > s) f
  //    else s
  //  }

  /*
  EXERCISE 3.28
  Write a function map, analogous to the method of the same name on List, that modifies
  each element in a tree with a given function.
  */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    //    @annotation.tailrec
    def loop(tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value) => Leaf(f.apply(value))
      case Branch(left, right) => Branch(loop(left)(f), loop(right)(f))
    }

    loop(t)(f)
  }

  /*
  EXERCISE 3.29
  Generalize size, maximum, depth, and map, writing a new function fold that abstracts
  over their similarities. Reimplement them in terms of this more general function. Can
  you draw an analogy between this fold function and the left and right folds for List?
   */
}
