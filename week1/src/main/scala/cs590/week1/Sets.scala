package cs590.week1

/**
  * Problem 1: Intro to Scala
  *
  * (largely taken from "Functional Programming Principles in Scala"
  * tought at EPFL and on Coursera)
  */

trait FunSets {

  /**
    * This type alias defines how sets are represented.
    */
  type Set = Int => Boolean

  protected val elements =
    (s: Set) => for (i <- -1000 to 1000 if contains(s, i)) yield(i)

  /**
    * This function tests whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * This function displays the contents of a set.
    */
  def toString(s: Set): String = {
    val xs = for (i <- elements(s)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Constructs a single-element Set that contains the specified element.
    */
  def set(elem: Int): Set = _ == elem

  /**
    * Constructs a union of the two specified sets.
    */
  def union(s: Set, t: Set): Set = (elem: Int) => s(elem) || t(elem)

  /**
    * Constructs an intersection of the two specified sets.
    */
  def intersect(s: Set, t: Set): Set = (elem: Int) => s(elem) && t(elem)

  /**
    * Constructs a difference of the two specified sets.
    */
  def diff(s: Set, t: Set): Set = (elem: Int) => s(elem) && !t(elem)

  /**
    * Constructs a new set out of the specified set such that only
    * elements that pass the filter are retained.
    */
  def filter(s: Set, p: Int => Boolean): Set =
    (elem: Int) => p(elem) && contains(s, elem)

  def forall(s: Set, p: Int => Boolean): Boolean = {
    val xs = for (i <- elements(s)) yield p(i)
    xs.foldLeft(true)(_ && _)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, !p(_))

  def map(s: Set, f: Int => Int): Set = {
    // Note: if inverse of w was given, it would be simply: (x: Int) => s(f(x))
    val xs = for (i <- elements(s)) yield f(i)
    xs.contains(_)
  }
}


trait IntSets {

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean

    /**
      * TODO: implement missing functionality (Bonus)
      */

    // def intersect(that: IntSet): IntSet
    // def intersect0(that: IntSet, accu: IntSet): IntSet

    // def filter(p: Int => Boolean): IntSet

    // def intersect2(that: IntSet): IntSet
  }

  class Empty extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

    override def toString() = "Empty"
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if (x < elem) left.contains(x)
      else if (x > elem) right.contains(x)
      else true

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left.incl(x), right)
      else if (x > elem) new NonEmpty(elem, left, right.incl(x))
      else this

    override def toString() = "NonEmpty(%d, %s, %s)".format(elem, left, right)
  }

}
