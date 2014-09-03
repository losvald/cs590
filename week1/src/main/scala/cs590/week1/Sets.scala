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

    // The first thing that comes to my mind is to use pattern matching.y
    // However, I would need to make the derived classes be "case classes",
    // or at least implement unapply method for each (see below). Then it'd be:
    // def intersect(that: IntSet): IntSet = (this, that) match {
    //   case (Empty, _) => this
    //   case (_, Empty) => that
    //   case (s1 : NonEmpty(e1, l1, r1), s2 : NonEmpty(e2, l2, r2)) => ...
    //   // otherwise, the derived class probably knows to intersect better
    //   case (_, _) => this.intersect(that)    // invoke virtual method
    // }
    // Anyway, I guess the indended solution is to use polymorphism only

    def intersect(that: IntSet): IntSet = intersect0(that, new Empty)

    def filter(p: Int => Boolean): IntSet = this

    // TODO: How do we hide these auxilliary methods?
    // (protected doesn't work here for some reason)

    def intersect2(that: IntSet) =
      filter(elem => this.contains(elem) && that.contains(elem))

    def intersect0(that: IntSet, accu: IntSet): IntSet

    // We could avoid this method by using "case left/right : NonEmpty/Empty",
    // but that would be an anti-pattern w.r.t. OOP (and also less efficient)
    def filter0(p: Int => Boolean, accu: IntSet): IntSet
  }

  class Empty extends IntSet {
    override def contains(x: Int): Boolean = false
    override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

    override def intersect0(that: IntSet, accu: IntSet) = accu

    override def filter(p: Int => Boolean) = this
    override def filter0(p: Int => Boolean, accu: IntSet) = accu

    override def toString() = "Empty"
  }

  // TODO: Restrict access to the ctor to Empty only (and this class)?
  // Make it protected and wrap it in a package???
  class NonEmpty(elem: Int, left: IntSet = new Empty, right: IntSet = new Empty)
      extends IntSet {

    // TODO: I tried to implement the "unapply" method, but I got stuck :(
    //   def unapply(s: NonEmpty): Option[(Int, IntSet, IntSet)] = ???y

    override def contains(x: Int): Boolean =
      if (x < elem) left.contains(x)
      else if (x > elem) right.contains(x)
      else true

    override def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left.incl(x), right)
      else if (x > elem) new NonEmpty(elem, left, right.incl(x))
      else this

    override def intersect0(that: IntSet, accu: IntSet) =
      right.intersect0(
        that,
        left.intersect0(
          that,
          if (that.contains(elem)) accu.incl(elem) else accu
        )
      )

    override def filter(p: Int => Boolean) = filter0(p, new Empty)

    override def filter0(p: Int => Boolean, accu: IntSet) =
      right.filter0(p, left.filter0(p, if (p(elem)) accu.incl(elem) else accu))

    override def toString() = "NonEmpty(%d, %s, %s)".format(elem, left, right)
  }

}
