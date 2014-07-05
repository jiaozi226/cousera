package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\t" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  // def filter(p: Tweet => Boolean): TweetSet = ???
   def filter(p: Tweet => Boolean): TweetSet = {
	 	 val empty = new Empty;
		 this.filterAcc(p, empty);
	 }

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
   // def union(that: TweetSet): TweetSet = ???
   def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet;

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  // def descendingByRetweet: TweetList = ???
  def descendingByRetweet: TweetList


  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
		acc
	}
  def descendingByRetweet: TweetList = {
		Nil
	}


  def union(that: TweetSet): TweetSet = {
		that
	}
  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
  def mostRetweeted: Tweet = {
  	throw new java.util.NoSuchElementException("head of EmptyList")
	}
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
	def top:Tweet = elem
	def lchild:TweetSet = left
	def rchild:TweetSet = right

  // def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = ???
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
		val accLeft = left.filterAcc(p, acc);
		val accRight = right.filterAcc(p, accLeft);
		if (p(elem)) {
			accRight.incl(elem);
		} else {
			accRight
		}
	}
 
  def descendingByRetweet: TweetList = {
		val h = this.mostRetweeted
		new Cons(h,this.remove(h).descendingByRetweet)
	}
  def mostRetweeted: Tweet = {
		val firstRes = if(this.lchild.isInstanceOf[NonEmpty]) {
			val leftMost = this.lchild.mostRetweeted;
			if (this.top.retweets < leftMost.retweets) {
				leftMost
			} else {
				this.top
			}
		} else {
			this.top
		}
		if (this.rchild.isInstanceOf[NonEmpty]) {
			val rightMost = this.rchild.mostRetweeted;
			if (rightMost.retweets < firstRes.retweets) {
				firstRes
			} else {
				rightMost
			}
		} else {
			firstRes
		}
	}
  def union(that: TweetSet): TweetSet = {
		if (that.isInstanceOf[Empty]) this
		else {
			val other = that.asInstanceOf[NonEmpty]; 
			val lessSet = if (this.top.text <= other.top.text) this else other
			val greaterSet = if (this.top.text <= other.top.text) other else this
			val newLeft = {
				val tmp = lessSet.lchild.union(greaterSet.lchild)
				if (lessSet.top.text == greaterSet.top.text) {
					tmp
				} else {
					tmp.incl(lessSet.top)
				}
			}
			// println("lessSet top:" + lessSet.top.user +
			// "  greaterSet top:" + greaterSet.top.user);
			// println("NewLeft:");
			// newLeft.descendingByRetweet foreach println
			// println("NewRight:");
			// greaterSet.rchild.descendingByRetweet foreach println
			// println("Another to union:");
			// lessSet.rchild.descendingByRetweet foreach println
			// println("")
			new NonEmpty(greaterSet.top, newLeft, greaterSet.rchild).union(lessSet.rchild);
		}
	}
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(t=>{google.exists(keyword=>{t.text.contains(keyword)})})
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(t=>{apple.exists(keyword => {t.text.contains(keyword)})})

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  // GoogleVsApple.trending foreach println
  val set1 = new Empty
  val set2 = set1.incl(new Tweet("a", "a body", 20))
  val set3 = set2.incl(new Tweet("b", "b body", 20))
  val c = new Tweet("c", "c body", 7)
  val d = new Tweet("d", "d body", 9)
  val set4c = set3.incl(c)
  val set4d = set3.incl(d)

  val set5 = new Empty().incl(new Tweet("a", "a body", 20))
  val set6 = set5.incl(new Tweet("b", "b body", 20))
  val set7 = new Empty().incl(new Tweet("c", "c body", 7))

	// set6.asInstanceOf[NonEmpty].rchild.descendingByRetweet foreach println
  // set6.union(set7).descendingByRetweet foreach println
	// println("");

	// set4c.union(set4d).descendingByRetweet foreach println
}
