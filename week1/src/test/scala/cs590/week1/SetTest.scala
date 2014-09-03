package cs590.week1

import org.scalatest.FunSuite


class FunSetTest extends FunSuite with FunSets {

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */

  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }


  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the
    * values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  test("set contains") {
    assert(contains((x: Int) => x == 1, 1))
  }

  test("set(1) contains 1") {
    assert(contains(set(1), 1))
  }

  test("set(1) does not contain 2") {
    assert(!contains(set(1), 2))
  }

  test("set(42) contains 42") {
    assert(contains(set(42), 42))
  }

  test("union contains all elements") {
    val s = union(set(1), set(2))
    assert(contains(s, 1), "Union 1")
    assert(contains(s, 2), "Union 2")
    assert(!contains(s, 3), "Union 3")
  }

  test("intersect same singleton") {
    val s = intersect(set(3), set(3))
    assert(contains(s, 3), "contains common")
    assert(!contains(s, 1), "does not contain missing")
  }

  test("intersect different singleton") {
    val s = intersect(set(2), set(3))
    assert(!contains(s, 2), "does not contain 2")
    assert(!contains(s, 3), "does not contain 3")
  }

  test("filter passes some") {
    val s = union(union(set(1), set(2)), set(3))
    val sLt2 = filter(s, _ < 2)
    assert(contains(sLt2, 1), "contains 1")
    assert(!contains(sLt2, 2), "does not contain 2")
    assert(!contains(sLt2, 3), "does not contain 3")
    assert(!contains(sLt2, 0), "false positive sanity-check")
  }

  test("filter passes all") {
    val s = union(union(set(1), set(2)), set(3))
    val sGt0 = filter(s, _ > 0)
    assert(contains(sGt0, 1), "contains 1")
    assert(contains(sGt0, 2), "contains 2")
    assert(contains(sGt0, 3), "contains 3")
  }

  test("filter passes none") {
    val s = union(union(set(1), set(2)), set(3))
    val sEmpty = filter(s, _ => false)
    assert(!contains(sEmpty, 1), "does not contain 1")
    assert(!contains(sEmpty, 2), "does not contain 2")
    assert(!contains(sEmpty, 3), "does not contain 3")
  }

  test("forall all tautology") {
    assert(forall(union(set(1), set(2)), _ => true), "tautology")
  }

  test("forall all true") {
    assert(forall(union(set(1), set(2)), _ < 3), "all true")
    assert(forall(set(42), _ == 42), "all true (singleton)")
  }

  test("forall some true") {
    assert(!forall(union(set(1), set(2)), _ == 1), "first true only")
    assert(!forall(union(set(1), set(2)), _ == 2), "last true only")
    assert(
      !forall(union(set(4), union(set(3), set(8))), _ != 3),
      "all but one true")
  }

  test("forall all contradiction") {
    assert(!forall(union(set(4), set(2)), _ => false), "contradiction")
  }

  test("forall all false") {
    assert(!forall(union(set(4), set(2)), _ < 1), "all false")
  }

  test("forall empty") {
    val sEmpty = intersect(set(2), set(3))
    assert(forall(sEmpty, _ => true), "empty tautology")
    assert(forall(sEmpty, _ => false), "empty contradiction")
  }

  test("exists all tautology") {
    assert(exists(set(1), _ => true), "all true")
    assert(exists(union(set(1), set(2)), _ => true), "all true")
  }

  test("exists some true") {
    assert(exists(union(set(1), set(2)), _ == 1), "first true")
    assert(exists(union(set(1), set(2)), _ == 2), "last true")
    assert(
      exists(union(set(42), union(set(7),  union(set(5), set(13)))), _ < 8),
      "middle true")
  }

  test("exists contradiction") {
    assert(!exists(union(set(1), set(2)), _ => false), "contradiction")
  }

  test("exists all false") {
    assert(!exists(union(set(7), set(1)), _ > 9), "all false")
  }

  test("exists empty") {
    val sEmpty = intersect(set(2), set(3))
    assert(!exists(sEmpty, _ => true), "empty tautology")
    assert(!exists(sEmpty, _ => false), "empty contradiction")
  }

  test("map injective") {
    val s = union(set(1), set(2))
    val sMapped = map(s, _ * 3)
    assert(contains(sMapped, 3), "contains 3")
    assert(contains(sMapped, 6), "contains 6")
    assert(
      forall(sMapped, (x: Int) => x == 3 || x == 6),
      "contains nothing except 3 and 6")
  }

  test("map non-injective") {
    assert(
      forall(map(union(set(3), set(5)), _ / 3), _ == 1),
      "both maps to one")
  }

  test("map identity") {
    val s = union(set(4), set(2))
    val id = (x: Int) => x
    val sMapped = map(s, id)
    assert(contains(sMapped, 4), "identity doesn't remove 4")
    assert(contains(sMapped, 2), "identity doesn't remove 2")
    assert(
      !exists(sMapped, (x: Int) => x != 4 && x != 2),
      "identity doesn't add")
    assert(toString(sMapped) == toString(s), "toString unaffected")
  }
}


class IntSetTest extends FunSuite with IntSets {

  val s2 = new NonEmpty(2)
  val s21 = s2.incl(1)
  val s214 = s21.incl(4)

  def assertContainsAllOf(msg: Any, s: IntSet, xs: Int*) =
    // TODO: I would like to propagate assertion failure to this method's caller
    // (it's inconvenient to pass around this "msg" parameter)
    for (x <- xs) assert(s.contains(x), msg)

  private def assertContains124(s: IntSet, msg: Any = "") =
    assertContainsAllOf(msg, s, 1, 2, 4)

  test("contains balanced") {
    assertContains124(s214)
  }

  val s24 = s2.incl(4)
  val s241 = s24.incl(1)

  test("contains balanced permuted children incl") {
    assertContains124(s241)
  }

  val s1 = new NonEmpty(1)
  val s12 = s1.incl(2)
  val s124 = s12.incl(4)
  val s14 = s1.incl(4)
  val s142 = s14.incl(2)

  val s4 = new NonEmpty(4)
  val s41 = s4.incl(1)
  val s412 = s41.incl(2)
  val s42 = s4.incl(2)
  val s421 = s42.incl(1)

  val s124Permutations = Traversable(s124, s142, s214, s241, s412, s421)

  test("contains permute root incl") {
    for (s <- s124Permutations)
      assertContains124(s)
  }

  def makeSet(xs: Int*) = xs.foldLeft[IntSet](new Empty)(_.incl(_))

  test("(meta) makeSet") {
    val fibUpTo13 = makeSet(1, 1, 2, 3, 5, 8, 13)
    assert(fibUpTo13.contains(3))
    assert(!fibUpTo13.contains(4))
    assert(fibUpTo13.contains(5))
  }

  test("intersect self") {
    val s = makeSet(3, 1, 4, 5, 9, 2, 6)
    assert(s.intersect(s).toString === s.toString)
  }

  test("intersect empty singletons") {
    val s = makeSet(1).intersect(makeSet(2))
    assert(!s.contains(1))
    assert(!s.contains(2))
  }

  def intersectMessage(s1 : IntSet, s2 : IntSet) =
    ": %s.intersect(%s)".format(s1, s2)

  test("intersect permuted incl") {
    val ss = Traversable(s124, s142, s214, s241, s412, s421)
    for (s1 <- ss; s2 <- ss) {
      assertContains124(s1.intersect(s2), intersectMessage(s1, s2))
      assertContains124(s2.intersect(s1), intersectMessage(s2, s1))
    }
  }

  def assertContainsNoneOf(msg: Any, s: IntSet, xs: Int*) =
    for (x <- xs) assert(!s.contains(x), msg)

  test("intersect common root") {
    val ss = Seq(makeSet(3, 1), makeSet(3, 2), makeSet(3, 4), makeSet(3, 5))
    for (s1 <- ss; s2 <- ss) {
      if (s1 != s2) {
        val root = s1.intersect(s2)
        assert(root.contains(3), intersectMessage(s1, s2))
        assertContainsNoneOf(intersectMessage(s1, s2), root, 1, 2, 4, 5)
      }
    }
  }

  test("intersect single common") {
    val sRootAndRight = makeSet(2, 4, 8).intersect(makeSet(0, 2, 6))
    assert(sRootAndRight.contains(2))
    assertContainsNoneOf("", sRootAndRight, 0, 4, 6, 8)

    val sLeftAndRightRightLeft = makeSet(9, 7).intersect(makeSet(0, 2, 8, 7))
    assert(sLeftAndRightRightLeft.contains(7))
    assertContainsNoneOf("", sLeftAndRightRightLeft, 0, 2, 8, 9)
  }

  test("intersect complex") {
    val s31459 = makeSet(3, 1, 4, 1, 5, 9)
    val s97531 = makeSet(9, 7, 5, 3, 1)
    val s12358 = makeSet(1, 2, 3, 5, 8)
    val s82153 = makeSet(8, 2, 1, 5, 3)

    for (s <- s124Permutations) {
      assert(s97531.intersect(s).toString === makeSet(1).toString, s)
      assertContainsAllOf(s.toString, s31459.intersect(s), 1, 4)
      for (sWith1and2 <- Seq(s12358, s82153)) {
        assertContainsAllOf(intersectMessage(s, sWith1and2),
          sWith1and2.intersect(s), 1, 2)
        assertContainsNoneOf(intersectMessage(s, sWith1and2),
          sWith1and2.intersect(s), 3, 4, 5, 6, 7, 8, 9)
      }
    }
  }

  test("intersect no common non-empty") {
    assert(makeSet(0, 2, 4, 6, 8).intersect(makeSet(1, 3, 5, 7, 9)).toString ===
      (new Empty).toString)
    assert(makeSet(7, 4, 1, 9, 2, 3, 5).intersect(makeSet(999, 99, 111, 33))
      .toString === (new Empty).toString)
    // TODO: find a better way to wrap the lines above
  }

  test("intersect no common empty") {
    def e() = new Empty()
    assert(e().intersect(e()).toString == e().toString)
  }

  test("filter some") {
    val s = makeSet(-10, 5, 21, -1, 0, 3)
    val sGt0 = s.filter(_ > 0)
    assertContainsAllOf("", sGt0, 5, 21, 3)
    assertContainsNoneOf("", sGt0, -10, -1, 0)
  }

  test("filter all") {
    val seqGt0 = Seq(3, 1, 4, 5, 9, 2, 6)
    assertContainsAllOf("", makeSet(seqGt0:_*).filter(_ > 0), seqGt0:_*)
  }

  test("filter none") {
    val seqGt0 = Seq(3, 1, 4, 5, 9, 2, 6)
    assertContainsNoneOf("", makeSet(seqGt0:_*).filter(_ < 0), seqGt0:_*)
    assertContainsNoneOf("", s12.filter(_ > 2), 1, 2)
    assert(makeSet().filter(_ => true).toString === makeSet().toString)
  }

  test("intersect2 permuted self") {
    for (s1 <- s124Permutations; s2 <- s124Permutations)
      if (s1 != s2) {
        val s = s1.intersect2(s2)
        val msg = intersectMessage(s1, s2)
        assertContains124(s, msg)
        assertContainsNoneOf(msg, s)
      }
  }

  test("intersect2 partial") {
    val s = makeSet(4, -5, 6, 2, 1, 7, 9, -4, -2, 8).intersect2(
      makeSet(3, -2, -3, 5, 0, -6, 8, 4, -7, 1))

    assertContainsAllOf("common", s, -2, 1, 4, 8)
    assertContainsNoneOf("1st only", s, -5, -4, 2, 6, 7, 9)
    assertContainsNoneOf("2nd only", s, -7, -6, -3, 0, 3, 5)
    assertContainsNoneOf("neither", s, -9, -99, 10)
  }

  test("intersect2 empty") {
    val sEmpty = new Empty
    assert(sEmpty.intersect2(sEmpty).toString === sEmpty.toString)
  }
}
