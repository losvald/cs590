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

  val s1 = new NonEmpty(2, new Empty, new Empty)
  val s2 = s1.incl(1)
  val s3 = s2.incl(4)

  test("s3 contains all elements") {
    assert(s3.contains(2), "Contains 1")
    assert(s3.contains(1), "Contains 2")
    assert(s3.contains(4), "Contains 3")
  }

  /**
    * TODO: add tests
    *
    * for s = {−10, 5, 21, −1, 0, 3},
    * s.filter(x => x > 0)
    * should return {5, 21, 3}
    */
}
