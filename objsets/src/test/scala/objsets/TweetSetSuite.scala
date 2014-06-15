package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import TweetReader._

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val gizmodo = TweetReader.allTweets.filter((t: Tweet) =>  t.user == "gizmodo")
    val googleVsApple = GoogleVsApple.trending
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
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

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
  
  test("descending: gizmodo") {
    new TestSets {
      assert(gizmodo.descendingByRetweet.length == 100)
      assert(gizmodo.descendingByRetweet.head.retweets == 321)
      assert(gizmodo.descendingByRetweet.tail.head.retweets == 290)
      assert(gizmodo.descendingByRetweet.tail.tail.head.retweets == 280)
      assert(gizmodo.descendingByRetweet.tail.tail.tail.head.retweets == 277)
      assert(gizmodo.descendingByRetweet.tail.tail.tail.tail.head.retweets == 242)
      assert(gizmodo.descendingByRetweet.tail.tail.tail.tail.tail.head.retweets == 202)
    }
  }
  
  test("descending: Google Vs Apple") {
    new TestSets {
      assert(googleVsApple.length == 179)
      assert(googleVsApple.head.retweets == 321)
      assert(googleVsApple.tail.head.retweets == 290)
      assert(googleVsApple.tail.tail.head.retweets == 205)
      assert(googleVsApple.tail.tail.tail.head.retweets == 202)
      assert(googleVsApple.tail.tail.tail.tail.head.retweets == 198)
      assert(googleVsApple.tail.tail.tail.tail.tail.head.retweets == 191)
      assert(googleVsApple.tail.tail.tail.tail.tail.tail.head.retweets == 180)
      assert(googleVsApple.tail.tail.tail.tail.tail.tail.tail.head.retweets == 139)
      assert(googleVsApple.tail.tail.tail.tail.tail.tail.tail.tail.head.retweets == 131)
    }
  }
 }
  
