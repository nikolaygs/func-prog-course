package forcomp

import com.sun.org.apache.xerces.internal.impl.xs.models.XSDFACM.Occurence
import com.sun.org.apache.xerces.internal.impl.xs.models.XSDFACM.Occurence


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    val wordGroupedByChars = w.toLowerCase.toList.groupBy(x => x).toList

    val result = for (x <- wordGroupedByChars) yield {
      val (char, occurList) = x
      (char, occurList.size)
    }

    result.sortBy(_._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {    
    wordOccurrences { 
      val result = s match {
        case Nil => ""
        case _ => s.reduce(_ + _)
      }
    
      result
    }
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val result = for (x <- dictionary) yield (wordOccurrences(x), x)
    val test = result.groupBy(x => x._1)

    for { 
      (occurr, listOccurrToWord) <- test
    } yield (occurr, listOccurrToWord.map(_._2))

  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = 
    dictionaryByOccurrences.get(wordOccurrences(word)).getOrElse(Nil)
 

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {

    def subOccurr(char: Char, x: Int) = (for (i <- 1 to x) yield (char, i)).toList

    val result = for ((char,x) <- occurrences) yield subOccurr(char, x)

//    Console println s"Result: ${result}"
    
    def combine(el: (Char, Int), list: List[(Char, Int)]): List[List[(Char, Int)]] = 
      list match {
        case Nil => (el :: Nil) :: Nil
        case x :: xs => (el :: x :: Nil) :: combine(el, xs)
      }

    def getFlatResult(input: List[List[(Char, Int)]]) = for {
      i <- input.head
      j <- combine(i, if (input.tail.isEmpty) Nil else input.tail.head)
    } yield j

    def loop(list: List[List[(Char, Int)]]): List[List[(Char, Int)]] = list match {
      case Nil => Nil
      case x :: Nil => getFlatResult(x :: Nil)
      case x :: xs => {
        val elem = x
        val cache = loop(xs)

//        Console println s"Cache: ${cache}"
//        Console println s"Head: ${x}"  

        val headToList =  for (el <- x) yield List(el)

        val res = for {
          head <- elem
          remain <- cache
        } yield head :: remain

        headToList ::: res ::: loop(xs)
      }
    }

    List() :: loop(result)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val otherMap = y.toMap

    x.foldLeft(Map[Char, Int]()) { (result, el) => 
      val (char, countX) = el

      if (!otherMap.contains(char)) result.updated(char, countX)
      else {
        val countY = otherMap.get(char).get
        val updatedValue = countX - countY

        if (updatedValue == 0) result
        else result.updated(char, updatedValue)
      } 
    }.toList.sortBy(_._1)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence )= 
    if (sentence.isEmpty) List(Nil)    
    else {
      val sentenceOccurr = sentenceOccurrences(sentence)
      sentenceAnagramsNonEmpty(sentenceOccurr)
    }

  private def getMatchedCombFromDict(sentenceOccurr: Occurrences) = 
    combinations(sentenceOccurr)
      .filter(dictionaryByOccurrences.contains(_))
      .map(x => (x, dictionaryByOccurrences(x)))

  private def sentenceAnagramsNonEmpty(sentenceOccurr: Occurrences): List[Sentence] = {
    val matched = getMatchedCombFromDict(sentenceOccurr)
    val matchedMap = matched.toMap
  
    def remainingValidComb(taken: Occurrences) = {
      val newRemainingChars = subtract(sentenceOccurr, taken)
      val remainComb = combinations(newRemainingChars)
      val remainValidComb = remainComb.filter(matchedMap.contains(_))
  
      remainValidComb.flatMap(x => matchedMap.get(x).get)
    }
  
    def initial(data: List[(Anagrams.Occurrences, List[Anagrams.Word])]) = 
      for {
        x <- data
        (takenChars, words) = x
        n <- words
        y <- remainingValidComb(takenChars)
      } yield {
        n :: y :: Nil
      }
  
    def findPossibleMatches(alreadyMatched: List[List[Word]]) = {
      for {
        x <- alreadyMatched
        taken = sentenceOccurrences(x)
        y <- remainingValidComb(taken)
      } yield y :: x
    }

    def filterMatches(alreadyMatched: List[List[Word]]) = 
      for {
        x <- alreadyMatched
        taken = sentenceOccurrences(x)
        if (taken == sentenceOccurr)
      } yield x


    def loop(data: List[List[Word]]): List[List[Word]] = {
      if (data.isEmpty) Nil
      else {
        val alreadyMatched = filterMatches(data)
        val possibleMatches = findPossibleMatches(data)
        alreadyMatched ::: loop(possibleMatches)
      }
    }
  
    val singleWordMatch = filterMatches(for (x <- matched; (_, words) = x) yield words)

    val result = initial(matched)
    singleWordMatch ::: loop(result)
  }

}
