package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val tweetA = new Tweet("a", "a body", 20)
    val tweetB = new Tweet("b", "b body", 20)

    val set1 = new Empty
    val set2 = set1.incl(tweetA)
    val set3 = set2.incl(tweetB)
    val tweetC = new Tweet("c", "c body", 7)
    val tweetD = new Tweet("d", "d body", 9)
    val set4c = set3.incl(tweetC)
    val set4d = set3.incl(tweetD)
    val set5 = set4c.incl(tweetD)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  test("filterAcc") {
    new TestSets {
      val p = (t: Tweet) => t.user == "a" || t.user == "b"
      val filteredSet = set5.filterAcc(p, new Empty)

      assert(size(filteredSet) == 2)
    }
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: nonEmpty.union(nonEmpty)") {
    new TestSets {
      val unionSet = set2.union(set3)
      assert(unionSet.contains(tweetA) && unionSet.contains(tweetB) && size(unionSet) == 2)
    }
  }

  test("union: nonEmpty.union(empty)") {
    new TestSets {
      val unionSet = set2.union(set1)
      assert(unionSet.contains(tweetA) && size(unionSet) == 1)
    }
  }

  test("union: empty.union(nonEmpty)") {
    new TestSets {
      val unionSet = set1.union(set2)
      assert(unionSet.contains(tweetA) && size(unionSet) == 1)
    }
  }

  test("union: empty.union(empty)") {
    new TestSets {
      val unionSet = set1.union(set1)
      assert(unionSet.isInstanceOf[Empty] && size(unionSet) == 0)
    }
  }

  test("union: nested union") {
    new TestSets {
      val unionSet = set1.union(set2).union(set3).union(set4c).union(set4d).union(set5).union(set1)
      assert(size(unionSet) == 4 && unionSet.contains(tweetA) && unionSet.contains(tweetB) &&
        unionSet.contains(tweetC) && unionSet.contains(tweetD))
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("findMostRetweeted on non-empty set") {
    new TestSets {
      assert(set5.mostRetweeted.retweets == 20)
    }
  }

  test("findMostRetweeted on empty set") {
    new TestSets {
      val thrown = intercept[Exception] {
        set1.mostRetweeted
      }
      assert(thrown.getMessage === "Calling mostRetweeted on an empty TweetSet.")
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("GoogleVsApple") {
    val allTweets = TweetReader.allTweets.mostRetweeted
    assert(allTweets.retweets == 345)
  }
}
