package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import quickcheck.IntHeap
import quickcheck.Heap

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  val rand = new scala.util.Random
  lazy val genHeap: Gen[H] =
    var h = empty
    val chance = rand.nextInt(100)
    if (chance % 5 == 0) then {}
    else h = generateANonEmptyHeap()
    const(h)

  def generateANonEmptyHeap(): H =
    var h = empty
    for i <- 20 to 1 by -1 yield {
      val randMultiplier = rand.nextInt(11)
      h = insert(i * randMultiplier, h)
    }
    h
  def makeAListInit (heap: H): List[A] =
    var result: List[A] = makeAList(heap, Nil)
    result
  def makeAList(heap: H, tempList: List[A]): List[A] =
    var list = tempList
    if isEmpty(heap) then {}
    else {
        val min = findMin(heap)
        val heapReduced = deleteMin(heap)
        list = min :: makeAList(heapReduced, list)
    }
    list

  def verifyHeapIsOrdered(list: List[A]): Boolean =
    list match
      case Nil => true
      case x :: xs => {
        xs match
          case Nil => true
          case _ => {
            if ord.gt(x, xs.head) then false
            else verifyHeapIsOrdered(xs)
          }
      }

  given Arbitrary[H] = Arbitrary(genHeap)

  property("if we insert the min element it will be the min element") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("when we insert 2 elems into empty Heap the smallest will be min") = forAll { (h: H) =>
    val x = rand.nextInt()
    val y = rand.nextInt()
    val m = insert(y, insert(x, empty))
    val min = ord.min(x, y)
    min == findMin(m)
  }

  property("When we parse the heap into a list, it's ordered") = forAll { (h: H) =>
    val list = makeAListInit(h)
    val ordered = verifyHeapIsOrdered(list)
    ordered == true
  }

  property("Min with high-rank trees") = forAll { (h: H) =>
    val h1 = generateANonEmptyHeap()
    val h2 = generateANonEmptyHeap()
    val together = meld(h1, h2)
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val minTotal = ord.min(min1, min2)
    val minTogether = findMin(together)
    minTotal == minTogether
  }

  property("When melding the final tree consists of both trees") = forAll { (h: H) =>
    val h1 = generateANonEmptyHeap()
    val h2 = generateANonEmptyHeap()
    val list1 = makeAListInit(h1)
    val list2 = makeAListInit(h2)
    val heapTogether = meld(h1, h2)
    val expectedSet = (list1 ::: list2).toSet
    val actualSet = makeAListInit(heapTogether).toSet
    expectedSet == actualSet
  }

  property("We meld an empty and non-empty map") = forAll { (h: H) =>
    val empt = empty
    val heap = {if isEmpty(h) then generateANonEmptyHeap() else h}
    meld(empt, heap) == heap
    meld(heap, empt) == heap
    meld(empt, empt) == empt
  }



