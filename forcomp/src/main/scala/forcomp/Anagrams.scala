package forcomp

import forcomp.Anagrams.{Occurrences, Word}

import scala.io.{Codec, Source}

object Anagrams extends AnagramsInterface:

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   * how often the character appears.
   * This list is sorted alphabetically w.r.t. to the character in each pair.
   * All characters in the occurrence list are lowercase.
   *
   * Any list of pairs of lowercase characters and their frequency which is not sorted
   * is **not** an occurrence list.
   *
   * Note: If the frequency of some character is zero, then that character should not be
   * in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   * Note: the uppercase and lowercase version of the character are treated as the
   * same character, and are represented as a lowercase character in the occurrence list.
   *
   * Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences =
    val stringOccurr: Map[Char, String] = w.groupBy((symbol: Char) => symbol.toLower)
    val intOccurr: Map[Char, Int] = stringOccurr.map((symb: Char, occ: String) => (symb, occ.length))
    intOccurr.toList.sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    var w1: Word = ""
    for w <- s
      yield w1 = w1.+(w)
    wordOccurrences(w1)


  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   * the words that have that occurrence count.
   * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   * For example, the word "eat" has the following character occurrence list:
   *
   * `List(('a', 1), ('e', 1), ('t', 1))`
   *
   * Incidentally, so do the words "ate" and "tea".
   *
   * This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy((w: Word) => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences.get(wordOccurrences(word)) match
      case None => Nil
      case Some(l: List[Word]) => l


  /** Returns the list of all subsets of the occurrence list.
   * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   * is a subset of `List(('k', 1), ('o', 1))`.
   * It also include the empty subset `List()`.
   *
   * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   * List(
   * List(),
   * List(('a', 1)),
   * List(('a', 2)),
   * List(('b', 1)),
   * List(('a', 1), ('b', 1)),
   * List(('a', 2), ('b', 1)),
   * List(('b', 2)),
   * List(('a', 1), ('b', 2)),
   * List(('a', 2), ('b', 2))
   * )
   *
   * Note that the order of the occurrence list subsets does not matter -- the subsets
   * in the example above could have been displayed in some other order.
   */
  def combinationsHelp(occurrences: Occurrences, accResult: Occurrences, currentList: List[Occurrences]):
  (Occurrences, List[Occurrences]) = {
    var result: Occurrences = accResult
    var currentList1 = currentList
    val (char: Char, int: Int) = occurrences.head
    for (i: Int) <- int to 0 by -1 yield
      occurrences.tail match
        case Nil =>
          result = (char, i) :: result
          val sortedResult: Occurrences = result.filter((charf, intf) => {intf != 0}).sorted
          sortedResult match
            case Nil =>
            case (xx :: xxs) => currentList1 = sortedResult :: currentList1
          result = result.filter({case (char1, _) => !char1.equals(char)})
        case (x :: xs) =>
          result = result.filter({case (char1, _) => !char1.equals(char)})
          result = (char, i) :: result
          currentList1 = combinationsHelp(occurrences.tail, result, currentList)._2 ::: currentList1
    (result, currentList1)
  }

     /*
     for 0 to 2
        for 0 to 2
          for 0 to 3
              (a, b, c)
     */

  def combinations(occurrences: Occurrences): List[Occurrences] =
    var result: List[Occurrences] = Nil :: Nil
    occurrences match
      case Nil => result
      case (x :: xs) => {
        result = combinationsHelp(occurrences, Nil, result)._2 ::: result
        result = occurrences.filter((char1, int1) => {
          int1 != 0
        }) :: result
        result.toSet.toList
      }


  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   * The precondition is that the occurrence list `y` is a subset of
   * the occurrence list `x` -- any character appearing in `y` must
   * appear in `x`, and its frequency in `y` must be smaller or equal
   * than its frequency in `x`.
   *
   * Note: the resulting value is an occurrence - meaning it is sorted
   * and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap: Map[Char, Int] = x.toMap
    val yMap: Map[Char, Int] = y.toMap
    var zMap: Map[Char, Int] = xMap
    for (char, int) <- y yield {
      xMap.get(char) match
        case None => throw new NoSuchElementException()
        case Some(int: Int) => {
          val delta: Int = xMap(char) - yMap(char)
          if delta >= 0 then
            zMap = zMap + (char -> delta)
          else throw new ArithmeticException()
        }
    }
    zMap.toList.filter((char1, int1) => {
      int1 != 0
    }).sorted
  }

  def addOccurences(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap: Map[Char, Int] = x.toMap
    val yMap: Map[Char, Int] = y.toMap
    var zMap: Map[Char, Int] = xMap
    for (char, int) <- y yield {
      xMap.get(char) match
        case None => zMap = zMap + (char -> int)
        case Some(int: Int) => {
          val sum: Int = xMap(char) + yMap(char)
          zMap = zMap + (char -> sum)
        }
    }
    zMap.toList.sorted
  }

    /** Returns a list of all anagram sentences of the given sentence.
     *
     * An anagram of a sentence is formed by taking the occurrences of all the characters of
     * all the words in the sentence, and producing all possible combinations of words with those characters,
     * such that the words have to be from the dictionary.
     *
     * The number of words in the sentence and its anagrams does not have to correspond.
     * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
     *
     * Also, two sentences with the same words but in a different order are considered two different anagrams.
     * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
     * `List("I", "love", "you")`.
     *
     * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
     *
     * List(
     * List(en, as, my),
     * List(en, my, as),
     * List(man, yes),
     * List(men, say),
     * List(as, en, my),
     * List(as, my, en),
     * List(sane, my),
     * List(Sean, my),
     * List(my, en, as),
     * List(my, as, en),
     * List(my, sane),
     * List(my, Sean),
     * List(say, men),
     * List(yes, man)
     * )
     *
     * The different sentences do not have to be output in the order shown above - any order is fine as long as
     * all the anagrams are there. Every returned word has to exist in the dictionary.
     *
     * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
     * so it has to be returned in this list.
     *
     * Note: There is only one anagram of an empty sentence.
     */
    def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
      var sentenceAnagramsResult: List[Sentence] = Nil
      sentence match
        case Nil => sentenceAnagramsResult = List(Nil)
        case (x :: xs) => {
          val sentenceOcc: Occurrences = sentenceOccurrences(sentence)
          val combinList: List[Occurrences] = combinations(sentenceOcc)
          val m: Map[Occurrences, List[Word]] = smallDictionary(sentenceOcc, combinList)
          val smallDictionayCombin: List[Occurrences] = m.keySet.toList
          val sentenceComb : List[List[Occurrences]] = getAllSentenceCombinations(sentenceOcc,smallDictionayCombin)
          val sentenceCombinationsWithPermutations : List[List[Occurrences]] =
            getAllSentenceCombinationsWithPermutations(sentenceComb)
          val result = sentenceAnagramsHelper(sentenceCombinationsWithPermutations)
          sentenceAnagramsResult = result.filter((y : Sentence) => !y.equals(List()))
        }
      sentenceAnagramsResult
    }

    def smallDictionary(sentenceOcc: Occurrences, combinList: List[Occurrences]) : Map[Occurrences, List[Word]] =
      var m: Map[Occurrences, List[Word]] = Map.empty[Occurrences, List[Word]]
      for occ1 <- combinList yield
        dictionaryByOccurrences.get(occ1) match
          case None => m + (occ1 -> Nil)
          case Some(l: List[Word]) => m = m + (occ1 -> l)
      m

    def getAllSentenceCombinations(sentenceOcc : Occurrences, combinList: List[Occurrences]) :  List[List[Occurrences]] =
      var result: List[List[Occurrences]] = Nil
      combinList match
        case Nil => result
        case (x :: xs) => {
          var combinOnheNull: List[Occurrences] = combinList.filter(x => !x.equals(List()))
          val temp: Set[List[Occurrences]] = Set.empty
          val sentenceHelper: Set[List[Occurrences]] =
            getAllSentenceCombinationsHelper(sentenceOcc, combinOnheNull, Nil, Nil, temp)
          result = sentenceHelper.toList ::: result
          result = List() :: result
          result
        }

    def getAllSentenceCombinationsHelper(sentenceOcc : Occurrences, intermediateCombinList: List[Occurrences],
                                         intermediateAccList: List[Occurrences],
                                         intermediateAccOccurrences: Occurrences,
                                         intermediateResult: Set[List[Occurrences]]) :
    Set[List[Occurrences]] =
      var accList: List[Occurrences] = intermediateAccList
      var accOccurrences: Occurrences = intermediateAccOccurrences
      var result: Set[List[Occurrences]] = intermediateResult
      var combinList : List[Occurrences] = intermediateCombinList
//      println("combinList before loop = " + combinList.toString())
      combinList = combinList.filter(x => !accList.contains(x))
      combinList match
        case Nil => result
        case (xx :: xxs) => {
            for {occ <- combinList if !accList.contains(occ)} yield {
              accOccurrences = addOccurences(occ, accOccurrences)
              accList = occ :: accList
              try {
                val substract: Occurrences = subtract(sentenceOcc, accOccurrences)
                substract match
                  case Nil => {
                    result = result + accList
                    combinList = combinList.filter(x => !accList.contains(x))
                  }
                  case (x :: xs) => {
                    combinList = combinList.filter(x => !accList.contains(x))
                    val nextIteration = getAllSentenceCombinationsHelper(sentenceOcc, combinList,
                      accList, accOccurrences, result)
                    result = result ++ nextIteration
                  }
              } catch {
                case e: (NoSuchElementException | ArithmeticException) => {
                  combinList = combinList.filter(x => !accList.contains(x))
                }
              }
              finally {
                accList = accList.filter(x => !x.equals(occ))
                accOccurrences = subtract(accOccurrences, occ)
              }
            }
          result
        }

    def getAllSentenceCombinationsWithPermutations(sentenceComb : List[List[Occurrences]]) :
    List[List[Occurrences]] =
      val sentenceCombTemp = sentenceComb
      var iterator: Iterator[List[Occurrences]] = Iterator.empty[List[Occurrences]]
      var result: List[List[Occurrences]] = Nil
      sentenceCombTemp.foreach((list: List[Occurrences]) => {
        iterator = list.permutations
        val sentenceCombWithPermutations: List[List[Occurrences]] = iterator.toList
        println(sentenceCombWithPermutations.toString())
        result = sentenceCombWithPermutations ::: result
      })
//      println(result.toString())
      result

    def sentenceAnagramsHelper(sentenceCombinations: List[List[Occurrences]]) : List[Sentence] =
      val sentenceCombins = sentenceCombinations
      var result: List[Sentence] = Nil
      for (combination <- sentenceCombins) yield {
          val sentencesForComb: List[Sentence] = getSentencesForComb(combination)
          result = sentencesForComb ::: result
        }
      result

    def getSentencesForComb(combination: List[Occurrences]) : List[Sentence] =
      var occurrencesWordList : List[List[Word]] = Nil
      for (occ <- combination) yield {
        val occWord: Word = occurrencesToWord(occ)
        val wordAnagramsList: List[Word] = wordAnagrams(occWord)
        occurrencesWordList = wordAnagramsList :: occurrencesWordList
      }
      val result: List[Sentence] = formSentences(occurrencesWordList, Nil, Nil)._2
      result

    def formSentences(occWordList: List[List[Word]],
                      intermediateCurrentSentence: Sentence,
                      intermediateResult: List[Sentence]):
    (Sentence, List[Sentence]) =
      var result : List[Sentence] = intermediateResult
      var currentSentence: Sentence = intermediateCurrentSentence
      occWordList match
        case Nil => {
          result = currentSentence :: result
          (currentSentence,result)
        }
        case (x :: xs) => {
          val currentWordAnagrams = occWordList.head
          for (word <- currentWordAnagrams) yield {
              currentSentence = word :: currentSentence
              val (currSentence: Sentence, currResult: List[Sentence]) =
                formSentences(occWordList.tail, currentSentence, result)
              currentSentence = currSentence.filter(x => !x.equals(word))
              result = currResult
          }
        }
      (currentSentence, result)

    def occurrencesToWord(occurrences: Occurrences) : Word =
      var w: Word = ""
      var occChars: List[Char] = Nil
      occurrences.foreach((char, int) => {
        for i <- 1 to int by 1 yield
          occChars = char :: occChars
      })
      w = occChars.reverse.mkString
      w




object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()
