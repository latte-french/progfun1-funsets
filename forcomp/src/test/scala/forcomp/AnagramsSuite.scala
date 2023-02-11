package forcomp

import scala.collection.immutable.List

class AnagramsSuite extends munit.FunSuite:
  import Anagrams.*

  test("wordOccurrences: empty word") {
    assertEquals(wordOccurrences(""), List())
  }

  test("wordOccurrences: abcd (3pts)") {
    assertEquals(wordOccurrences("abcd"), List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert (3pts)") {
    assertEquals(wordOccurrences("Robert"), List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e (5pts)") {
    assertEquals(sentenceOccurrences(List("abcd", "e")), List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }


  test("dictionaryByOccurrences.get: eat (10pts)") {
    assertEquals(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet), Some(Set("ate", "eat", "tea")))
  }


  test("wordAnagrams married (2pts)") {
    assertEquals(wordAnagrams("married").toSet, Set("married", "admirer"))
  }

  test("wordAnagrams player (2pts)") {
    assertEquals(wordAnagrams("player").toSet, Set("parley", "pearly", "player", "replay"))
  }



  test("subtract: lard - r (10pts)") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assertEquals(subtract(lard, r), lad)
  }

  test("add: lard + r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val sum = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
    assertEquals(addOccurences(lard, r), sum)
  }


  test("combinations: [] (8pts)") {
    assertEquals(combinations(Nil), List(Nil))
  }

  test("combinations: abba (8pts)") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assertEquals(combinations(abba).toSet, abbacomb.toSet)
  }

  test("combinations: abbaccc") {
    val abbaccc = List(('a', 2), ('b', 2), ('c', 3))
    val abbaccccomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2)),
      List(('c', 3)),
      List(('c', 2)),
      List(('c', 1)),
      List(('a', 1), ('c', 3)),
      List(('a', 2), ('c', 3)),
      List(('b', 1), ('c', 3)),
      List(('a', 1), ('b', 1), ('c', 3)),
      List(('a', 2), ('b', 1), ('c', 3)),
      List(('b', 2), ('c', 3)),
      List(('a', 1), ('b', 2), ('c', 3)),
      List(('a', 2), ('b', 2), ('c', 3)),
      List(('a', 1), ('c', 2)),
      List(('a', 2), ('c', 2)),
      List(('b', 1), ('c', 2)),
      List(('a', 1), ('b', 1), ('c', 2)),
      List(('a', 2), ('b', 1), ('c', 2)),
      List(('b', 2), ('c', 2)),
      List(('a', 1), ('b', 2), ('c', 2)),
      List(('a', 2), ('b', 2), ('c', 2)),
      List(('a', 1), ('c', 1)),
      List(('a', 2), ('c', 1)),
      List(('b', 1), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 1)),
      List(('a', 2), ('b', 1), ('c', 1)),
      List(('b', 2), ('c', 1)),
      List(('a', 1), ('b', 2), ('c', 1)),
      List(('a', 2), ('b', 2), ('c', 1)),
    )
    assertEquals(combinations(abbaccc).toSet, abbaccccomb.toSet)
  }

  test("combinations: Yes man") {
    val yesman = List(('a', 1), ('e', 1), ('m', 1), ('n', 1), ('s', 1), ('y', 1))
    val yesManCom = combinations(yesman).toSet
    assert(yesman.nonEmpty)
  }


  test("sentence anagrams: [] (10pts)") {
    val sentence = List()
    assertEquals(sentenceAnagrams(sentence), List(Nil))
  }

  test("SentenceHelper") {
    val sentenceOcc : Occurrences = List(('a', 1), ('e', 1), ('m', 1), ('n', 1), ('s', 1), ('y', 1))
    val combinList: List[Occurrences] = List(
      List(('a',1), ('m',1)),
      List(('a',1), ('e',1), ('m',1)),
      List(('a',1), ('n',1), ('s',1)),
      List(('e',1), ('n',1)),
      List(('s',1), ('y',1)),
      List(('y',1)),
      List(('n',1), ('s',1), ('y',1)),
      List()
    )
    val result : Set[List[Occurrences]] = getAllSentenceCombinations(sentenceOcc, combinList).toSet
    val expectedResult: Set[List[Occurrences]] = Set(
      List(),
      List(List(('a',1), ('m',1)), List(('e',1), ('n',1)), List(('s',1), ('y',1))),
      List(List(('a',1), ('e',1), ('m',1)), List(('n',1), ('s',1), ('y',1)))
    )
    val resultFinal = result.foreach((x: List[Occurrences]) => x.toSet)
    val expectedFinal = expectedResult.foreach((x: List[Occurrences]) => x.toSet)
    assertEquals(resultFinal, expectedFinal)
  }

  test("sentence anagrams: Yes man (10pts)") {
    val sentence = List("Yes", "man")
    val anas = List(
      List("en", "as", "my"),
      List("en", "my", "as"),
      List("man", "yes"),
      List("men", "say"),
      List("as", "en", "my"),
      List("as", "my", "en"),
      List("sane", "my"),
      List("Sean", "my"),
      List("my", "en", "as"),
      List("my", "as", "en"),
      List("my", "sane"),
      List("my", "Sean"),
      List("say", "men"),
      List("yes", "man")
    )
    assertEquals(sentenceAnagrams(sentence).toSet, anas.toSet)
  }

  test("sentence anagrams: Linux rulez (10pts)") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assertEquals(sentenceAnagrams(sentence).toSet, anas.toSet)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
