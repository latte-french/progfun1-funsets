package patmat

import scala.collection.immutable.List

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val testCodeTree = Fork( //abcdefgh
      Leaf('a', 8),
      Fork( //bcdefgh
        Fork( //bcd
          Leaf('b', 3),
          Fork( //cd
            Leaf('c', 1),
            Leaf('d', 1),
            List('c', 'd'),
            2),
          List('b', 'c', 'd'),
          5),
        Fork( //efgh
          Fork( //ef
            Leaf('e', 1),
            Leaf('f', 1),
            List('e', 'f'),
            2),
          Fork( //gh
            Leaf('g', 1),
            Leaf('h', 1),
            List('g', 'h'),
            2),
          List('e', 'f', 'g', 'h'),
          4),
        List('b', 'c', 'd', 'e', 'f', 'g', 'h'),
        9),
      List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
      17)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a','b','d'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times with a string") {
    assertEquals(times(List('d', 'e', 'a', 'd', 'a', 'a', 'c')), List(('d', 2), ('e', 1), ('a', 3), ('c', 1)))
  }

  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("check if the list of trees is a singleton") {
    new TestTrees:
      val combined = List(t1, t2)
      assertEquals(singleton(combined), false)
      assertEquals(singleton(List(t1)), true)
  }

  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val leafList2 = List(Leaf('t', 2), Leaf('x', 4),Leaf('e', 1))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    assertEquals(combine(leafList2), List(Leaf('e', 1), Fork(Leaf('t', 2), Leaf('x', 4), List('t', 'x'), 6)))
  }

  test("testing until") {
    val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val leafList2 = List(Leaf('t', 2), Leaf('x', 4), Leaf('e', 1))
    val resultingTree1 = until(leafList => singleton(leafList), leafList =>
      combine(leafList))(leafList)
    val resultingTree2 = until(leafList2 => singleton(leafList2), leafList2 =>
      combine(leafList2))(leafList2)
    assertEquals(resultingTree1, List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e','t','x'), 7)))
    assertEquals(resultingTree2, List(Fork(Leaf('e', 1), Fork(Leaf('t', 2), Leaf('x', 4), List('t', 'x'), 6), List('e','t','x'), 7)))
  }

  test("encoding, let's try") {
    val charList : List[Char] = List('d', 'e', 'a', 'd', 'a', 'a', 'c')
    //List(('d', 2), ('e', 1), ('a', 3), ('c', 1))
    //List(Leaf('e', 1)), Leaf('c', 1), Leaf('d', 2), Leaf('a', 3)
    //List(Fork(Leaf('e', 1), Leaf('c',1), List('e','c'), 2), Leaf('d', 2), Leaf('a', 3))
    //List(Leaf('a', 3), Fork(Fork(Leaf('e', 1), Leaf('c',1), List('e','c'), 2),  Leaf('d', 2), List('e', 'c', d'), 4))
    val expectedTree = Fork(Leaf('a',3), Fork(Fork(Leaf('e', 1), Leaf('c',1), List('e','c'), 2),  Leaf('d', 2), List('e','c','d'), 4), List('a','e','c','d'), 7)
    val created = createCodeTree(charList)
    assertEquals(createCodeTree(charList), expectedTree)
  }

  test("try decoding for D") {
    new TestTrees:
      assertEquals(decode(testCodeTree, List(1, 0, 1, 1)), List('d'))
  }

  test("try decoding for F") {
    new TestTrees:
      assertEquals(decode(testCodeTree, List(1, 1, 0, 1)), List('f'))
  }

  test("try decoding for both") {
    new TestTrees:
      assertEquals(decode(testCodeTree, List(1, 0, 1, 1, 1, 1, 0, 1)), List('d', 'f'))
  }

  test("decoding french") {
    assertEquals(decodedSecret,List('h','u','f','f','m','a','n','e','s','t','c','o','o','l'))
  }

  test("try encoding for some word") {
    new TestTrees:
      assertEquals(encode(testCodeTree)(List('d','e','d','a','b')), List(1,0,1,1,1,1,0,0,1,0,1,1,0,1,0,0))
  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

  test("test Huffman table") {
    new TestTrees:
      var resultForT1: List[(Char, List[Bit])] = Huffman.convert(t1)
      assertEquals(resultForT1, List(('a', List(0)), ('b', List(1))))
  }

  test("test Huffman table, more complicated") {
    new TestTrees:
      var resultForT2: List[(Char, List[Bit])] = Huffman.convert(t2)
      assertEquals(resultForT2, List(('b', List(0,1)), ('a', List(0,0)), ('d', List(1))))
  }

  test("try quick encode via Table") {
    new TestTrees:
      assertEquals(quickEncode(testCodeTree)(List('d', 'e', 'd', 'a', 'b')), List(1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0))
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
